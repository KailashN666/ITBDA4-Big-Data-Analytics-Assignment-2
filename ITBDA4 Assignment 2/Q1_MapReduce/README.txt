ITBDA4-14 Assignment 2 - Question 1
MapReduce keyword search (row and column locator)
Kailash Nagasar - EDUV11298859

WHAT IT DOES
Searches a corpus of documents for a keyword and reports the row (line number)
and column (word position in that line) of every occurrence in each document.
The search is case insensitive and the keyword is passed in at run time.

FILES
  src/KeywordSearch.java          Mapper (Q1.2), Reducer (Q1.3), Job config (Q1.4)
  KeywordSearch.jar               built jar
  Q1 MapReduce - R Version.R      same logic in R, used to cross check the output
  output/keyword_africa.txt       result for the keyword "africa"
  output/keyword_SARS.txt         result for the keyword "SARS"
  output/Q1 Diagram.png           logical layout diagram
  output/Q1 Hadoop Evidence.png   cluster running plus both results
  output/R Version.png            R version console output

ENVIRONMENT
  Hadoop 3.3.6 pseudo distributed on Windows 11
  Eclipse Temurin JDK 11
  R 4.6.0

BUILD
  javac -cp "%HADOOP_HOME%\share\hadoop\common\*;%HADOOP_HOME%\share\hadoop\common\lib\*;%HADOOP_HOME%\share\hadoop\mapreduce\*" -d classes src\KeywordSearch.java
  jar -cf KeywordSearch.jar -C classes .

RUN ON HADOOP
  start-dfs.cmd
  start-yarn.cmd
  hdfs dfs -mkdir /input_dir
  hdfs dfs -put <path to map_reduce_test_docs>\*.txt /input_dir
  hadoop jar KeywordSearch.jar KeywordSearch /input_dir /output_dir africa
  hdfs dfs -cat /output_dir/part-r-00000

  The keyword is the third argument, so any word can be searched without
  rebuilding the jar. The output directory must not already exist.

RUN THE R VERSION
  Open in RStudio, set corpus_dir at the top, then Source.
  Or from the command line:
  Rscript "Q1 MapReduce - R Version.R" <corpus_dir> <keyword>

RESULT FOR "africa"
  doc1.txt  occurrences=1  locations=(1,10)
  doc2.txt  occurrences=2  locations=(1,9) (33,5)
  doc3.txt  occurrences=6  locations=(1,8) (40,22) (46,2) (50,23) (52,15) (54,6)
  doc4.txt  occurrences=0
  doc5.txt  occurrences=3  locations=(1,12) (3,14) (23,21)
  doc6.txt  occurrences=0
  doc7.txt  occurrences=0
