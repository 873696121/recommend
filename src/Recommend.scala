import scala.io.Source

object Recommend {

  val topN = 5
  val topK = 10

  def main(args: Array[String]): Unit = {
    println("starting loading matrix...")
    val file = "/Users/huhong/workspace/javaProjects/recommend/src/ratings.csv"
    val source = Source.fromFile(file)
    val seq = source.getLines().toSeq
    var N = 0
    var M = 0
    println("loading file success!")
    val res = seq.indices.filter(u => u != 0 && u < 100000).map(u => seq(u)).map(u => {
      val split = u.split(",")
      N = math.max(N, split(0).toInt)
      M = math.max(M, split(1).toInt)
      (split(0).toInt, split(1).toInt) -> split(2).toDouble
    }).toMap
    M = 200
    println(N + " " + M)
    val matrix = Seq.range(0, N).map(u => {
      Seq.range(0, M).map(v => {
        res.get((u, v)) match {
          case Some(value) => value
          case None => 0.0
        }
      })
    })
    val similarity = CosineUtil.calMatrixCosine(matrix, matrix)
    println(similarity.toString())
    val user_similarity = similarity.indices.map(u => u -> {
      val t = similarity(u).indices.map(v => (v, similarity(u)(v))).filter(t => t._1 != u).sortWith((a, b) => a._2 > b._2)
      t.indices.filter(v => v < topN).map(v => t(v))
    })
    user_similarity.foreach(u => {
      println(s"${u._1} : ${u._2}")
    })
  }
}
