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

// Question 1 - keyword search with MapReduce. Searches a corpus for a keyword and
// reports the row and column of every hit, where row is the line number in the
// document and column is the word position on that line.
// Run as: hadoop jar KeywordSearch.jar KeywordSearch /input_dir /output_dir africa

public class KeywordSearch {

    // The keyword comes in through the job config, it is not written into the code
    public static final String KEYWORD_PROPERTY = "search.keyword";

    // Marker for a document where the keyword never showed up
    public static final String NO_MATCH = "none";

    // Q1.2 - Mapper
    // In: one line of a document. Out: (document name, "row,column")
    public static class Map extends Mapper<LongWritable, Text, Text, Text> {

        private String keyword;
        private String documentName;
        private long lineNumber = 0;
        private long hitCount = 0;

        private final Text outKey = new Text();
        private final Text outValue = new Text();

        // setup runs once before the first map call. Pulling the keyword from the
        // config here is what makes the search term dynamic
        @Override
        protected void setup(Context context) throws IOException, InterruptedException {
            Configuration conf = context.getConfiguration();
            keyword = conf.get(KEYWORD_PROPERTY, "").toLowerCase();   // case insensitive

            // Work out which file this mapper is on so the reducer can group by document
            FileSplit split = (FileSplit) context.getInputSplit();
            documentName = split.getPath().getName();
        }

        @Override
        protected void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            lineNumber++;
            String line = value.toString();
            if (line.trim().isEmpty()) {
                return;                      // blank lines still use up a row number
            }

            String[] words = line.trim().split("\\s+");

            for (int col = 0; col < words.length; col++) {

                // Knock the punctuation off the ends so "Africa," still matches "africa"
                String token = words[col].replaceAll("^\\p{Punct}+|\\p{Punct}+$", "")
                                         .toLowerCase();
                if (token.isEmpty()) {
                    continue;
                }

                if (token.equals(keyword)) {
                    hitCount++;
                    outKey.set(documentName);
                    outValue.set(lineNumber + "," + (col + 1));   // 1 based
                    context.write(outKey, outValue);
                }
            }
        }

        // cleanup runs once after the last map call. A document with no hits emits
        // nothing at all, so it would just disappear from the output. The marker
        // keeps it in the report with a count of zero
        @Override
        protected void cleanup(Context context) throws IOException, InterruptedException {
            if (hitCount == 0) {
                outKey.set(documentName);
                outValue.set(NO_MATCH);
                context.write(outKey, outValue);
            }
        }
    }

    // Q1.3 - Reducer
    // In: (document name, every "row,column" for that document)
    // Out: (document name, how many times it appeared and where)
    public static class Reduce extends Reducer<Text, Text, Text, Text> {

        private final Text outValue = new Text();

        @Override
        protected void reduce(Text key, Iterable<Text> values, Context context)
                throws IOException, InterruptedException {

            List<long[]> positions = new ArrayList<long[]>();

            for (Text v : values) {
                String raw = v.toString();
                if (NO_MATCH.equals(raw)) {
                    continue;                // the zero hits marker, not a location
                }
                String[] rc = raw.split(",");
                positions.add(new long[]{ Long.parseLong(rc[0]), Long.parseLong(rc[1]) });
            }

            // Hadoop does not promise the values arrive in any order, so sort by row
            // then column, otherwise the output comes out different on every run
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

    // Q1.4 - Job configuration
    public static void main(String[] args) throws Exception {

        if (args.length < 3) {
            System.err.println("Usage: KeywordSearch <input_dir> <output_dir> <keyword>");
            System.exit(-1);
        }

        Configuration conf = new Configuration();

        // Push the keyword out to every mapper through the config. This is what
        // keeps it dynamic instead of hardcoded
        conf.set(KEYWORD_PROPERTY, args[2]);

        Job job = Job.getInstance(conf, "Keyword Search - row and column locator");
        job.setJarByClass(KeywordSearch.class);

        job.setMapperClass(Map.class);
        job.setReducerClass(Reduce.class);

        // Mapper output types
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(Text.class);

        // Final output types
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);

        // Row numbers are counted per split. Making the minimum split bigger than any
        // of the documents forces one split per file, so the line counter lines up
        // with the real line number in the document
        FileInputFormat.setMinInputSplitSize(job, 1024L * 1024L * 1024L);

        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
