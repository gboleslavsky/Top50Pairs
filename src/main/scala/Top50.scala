package top50
import scala.collection.mutable
import scala.io.Source

object Top50 extends App {
  val POPULARITY          = 50
  val LINES_IN_ONE_CHUNK  = 200

  val inputPath = "Artist_lists_small.txt"
  val outputPath = "Output_" + inputPath

  new PopularFavoriteArtists(inputPath, POPULARITY)  //line that runs everything

  class PopularFavoriteArtists(inputPath: String, popularity: Int) {
    type Line = String
    type Favorite = (String, String)

    var countsByFavorites = mutable.HashMap[Favorite, Int]()  //keeps temp MAP of not yet known to be popular pairs
    var popularFavorites =  mutable.MutableList[Favorite]()   //keeps pairs known to be popular

    //-------------------------- MAIN ---------------------------------------------
    for (line <- allArtistsAsListOfLines(inputPath)) {
      for (f <- favorites(artists(line)))
        if (! popularFavorites.contains(f))   //ignore pairs that are known to be popular already
          updateFavorites(f)
    }
    writeAnswer(outputPath)
    //-------------------------END OF MAIN ------------------------------------------

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
      if (currentPopularity >= popularity){
        popularFavorites  += f
        countsByFavorites -= f //no need to keep track of pairs already known to be popular
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
