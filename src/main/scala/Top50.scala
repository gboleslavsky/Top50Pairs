package top50
import scala.collection.mutable
import scala.io.Source

/*
Artist_lists_small.txt contains the favorite musical artists of 1000 users. Each line is a list of up to 50 artists, formatted as follows:
Radiohead,Pulp,Morrissey,Delays,Stereophonics,Blur,Suede,Sleeper,The La's,Super Furry Animals,Iggy Pop\n Band of Horses,Smashing Pumpkins,The Velvet Underground,Radiohead,The Decemberists,Morrissey,Television\n
etc.
The program below uses this file as input and produces an output file containing a list of pairs of artists which appear TOGETHER in at least fifty different lists. For example, in the above sample, Radiohead and
Morrissey appear together twice, but every other pair appears only once. The output file contains rows, each row being a pair. For example:
Morrissey,Radiohead\n
*/

object Top50 extends App {
  val POPULARITY =  50
  val inputPath =   "Artist_lists_small.txt"
  val outputPath =  "Output_" + inputPath

  new PopularFavoriteArtists(inputPath, POPULARITY).run //line that runs everything

  class PopularFavoriteArtists(inputPath: String, popularity: Int) {
    type Line = String
    type Favorite = (String, String)

    var countsByFavorites = mutable.HashMap[Favorite, Int]()  //keeps temp MAP of not yet known to be popular pairs
    var popularFavorites =  mutable.MutableList[Favorite]()   //keeps pairs known to be popular

    def run() = {
      for (line <- allArtistsAsListOfLines(inputPath)) {
        for (f <- favorites(artists(line)))
          if (!popularFavorites.contains(f)) //ignore pairs that are known to be popular already
            updateFavorites(f)
      }
      writeAnswer(outputPath)
    }

    def writeAnswer(outPath: String): Unit = {
      import java.io._
      val pw = new PrintWriter(new File(outPath))
      for (f <- popularFavorites.sorted) {
        pw.println( f._1 + ", " + f._2 )
      }
      pw.close
    }

    def updateFavorites(f: Favorite) = {
      val currentPopularity = currentCount(f)
      countsByFavorites(f) = currentPopularity + 1
      if (currentPopularity + 1 >= popularity){
        popularFavorites  += f
        countsByFavorites -= f    //no need to keep track of pairs already known to be popular
      }
    }

    def allArtistsAsListOfLines(path: String) = Source.fromFile(path).getLines.toList

    def artists(line: Line): List[String] = line.split(",").map(_.trim).toList

    def favorites(artists: Seq[String]): Seq[Favorite] =
      artists //pairs must come out with elements sorted with no duplicates
        .combinations(2) //all combinations of size 2
        .toList          //convert to List of pairs
        .map(l => if((l(0) >= l(1)))
          (l(1),l(0)) else ((l(0),l(1))))  //make sure pair elements are in fixed order, DESC alpha in this case
        .distinct        //remove duplicates

    def currentCount(f: Favorite): Int =
      countsByFavorites.get(f) match {
        case Some(count) => count
        case None => 0
      }
  }
}
