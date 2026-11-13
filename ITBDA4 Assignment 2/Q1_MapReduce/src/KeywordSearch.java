import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

/**
 * Question 1 - Hadoop MapReduce keyword search.
 *
 * Searches a corpus of documents for a keyword and reports the ROW and COLUMN
 * location of every occurrence in each document.
 *
 *   row    = line number within the document
 *   column = word position within that line
 *
 * The keyword is supplied at run time and is never hardcoded:
 *   hadoop jar KeywordSearch.jar KeywordSearch /input_dir /output_dir africa
 */
public class KeywordSearch {

    /** Configuration key under which the search term is published to the mappers. */
    public static final String KEYWORD_PROPERTY = "search.keyword";
    public static final String MATCHMODE_PROPERTY = "search.matchmode";

    /** Sentinel emitted for a document in which the keyword never appears. */
    public static final String NO_MATCH = "none";

    // =================================================================
    // Q1.2 - MAPPER
    // IN  : (byte offset, one line of the document)
    // OUT : (document name, "row,column")
    // =================================================================
    public static class Map extends Mapper<LongWritable, Text, Text, Text> {

        private String keyword;        // read once from the Job configuration
        private String matchMode;      // "word" (default) or "substring"
        private String documentName;   // the file this mapper instance is reading
        private long lineNumber = 0;   // running row counter for this split
        private long hitCount = 0;     // matches found in this document

        private final Text outKey = new Text();
        private final Text outValue = new Text();

        /**
         * setup() runs once before the first map() call. The keyword is pulled
         * from the configuration here rather than being written into the code,
         * which is what makes the search term dynamic.
         */
        @Override
        protected void setup(Context context) throws IOException, InterruptedException {
            Configuration conf = context.getConfiguration();
            keyword = conf.get(KEYWORD_PROPERTY, "").toLowerCase();   // case-insensitive
            matchMode = conf.get(MATCHMODE_PROPERTY, "word");

            // Identify which document this mapper is processing, so the reducer
            // can group the hits per document.
            FileSplit split = (FileSplit) context.getInputSplit();
            documentName = split.getPath().getName();
        }

        @Override
        protected void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            lineNumber++;                       // this line's row number
            String line = value.toString();
            if (line.trim().isEmpty()) {
                return;                         // blank lines still consume a row number
            }

            String[] words = line.trim().split("\\s+");

            for (int col = 0; col < words.length; col++) {

                // Strip leading/trailing punctuation so "Africa," still matches "africa"
                String token = words[col].replaceAll("^\\p{Punct}+|\\p{Punct}+$", "")
                                         .toLowerCase();
                if (token.isEmpty()) {
                    continue;
                }

                boolean hit = matchMode.equals("substring")
                        ? token.contains(keyword)
                        : token.equals(keyword);

                if (hit) {
                    // EMIT: key = document name, value = "row,column" (1-based)
                    hitCount++;
                    outKey.set(documentName);
                    outValue.set(lineNumber + "," + (col + 1));
                    context.write(outKey, outValue);
                }
            }
        }

        /**
         * cleanup() runs once after the last map() call. A document in which the
         * keyword never appears emits nothing, so Hadoop would produce no output
         * row for it at all. Emitting a sentinel here keeps every document in the
         * final report, including those with zero occurrences.
         */
        @Override
        protected void cleanup(Context context) throws IOException, InterruptedException {
            if (hitCount == 0) {
                outKey.set(documentName);
                outValue.set(NO_MATCH);
                context.write(outKey, outValue);
            }
        }
    }

    // =================================================================
    // Q1.3 - REDUCER
    // IN  : (document name, all "row,column" values for that document)
    // OUT : (document name, occurrence count followed by every location)
    // =================================================================
    public static class Reduce extends Reducer<Text, Text, Text, Text> {

        private final Text outValue = new Text();

        @Override
        protected void reduce(Text key, Iterable<Text> values, Context context)
                throws IOException, InterruptedException {

            List<long[]> positions = new ArrayList<long[]>();

            for (Text v : values) {
                String raw = v.toString();
                if (NO_MATCH.equals(raw)) {
                    continue;              // zero-occurrence marker, not a location
                }
                String[] rc = raw.split(",");
                positions.add(new long[]{ Long.parseLong(rc[0]), Long.parseLong(rc[1]) });
            }

            // Hadoop does not guarantee the order of the values, so sort by
            // row then column to make the output deterministic.
            Collections.sort(positions, new Comparator<long[]>() {
                public int compare(long[] a, long[] b) {
                    if (a[0] != b[0]) return Long.compare(a[0], b[0]);
                    return Long.compare(a[1], b[1]);
                }
            });

            StringBuilder sb = new StringBuilder();
            sb.append("occurrences=").append(positions.size()).append("  locations=");
            for (long[] p : positions) {
                sb.append("(").append(p[0]).append(",").append(p[1]).append(") ");
            }

            outValue.set(sb.toString().trim());
            context.write(key, outValue);
        }
    }

    // =================================================================
    // Q1.4 - HADOOP MAPREDUCE CONFIGURATION
    // =================================================================
    public static void main(String[] args) throws Exception {

        if (args.length < 3) {
            System.err.println("Usage: KeywordSearch <input_dir> <output_dir> <keyword> [word|substring]");
            System.exit(-1);
        }

        Configuration conf = new Configuration();

        // Publish the keyword to every mapper through the job configuration.
        // This is what keeps the search term dynamic instead of hardcoded.
        conf.set(KEYWORD_PROPERTY, args[2]);
        conf.set(MATCHMODE_PROPERTY, args.length >= 4 ? args[3] : "word");

        Job job = Job.getInstance(conf, "Keyword Search - row and column locator");
        job.setJarByClass(KeywordSearch.class);

        job.setMapperClass(Map.class);
        job.setReducerClass(Reduce.class);

        // Mapper output types
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(Text.class);

        // Reducer (final) output types
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);

        // Row numbers are counted per input split. Forcing a minimum split size
        // larger than any document guarantees one split per file, so the line
        // counter in the mapper matches the true line number in the document.
        FileInputFormat.setMinInputSplitSize(job, 1024L * 1024L * 1024L);

        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
