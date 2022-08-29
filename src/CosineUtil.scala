object CosineUtil {
  def module(vec: Seq[Double]) = {
    math.sqrt(vec.map(math.pow(_, 2)).sum)
  }

  def innerProduct(v1: Seq[Double], v2: Seq[Double]): Double = {
    if(v1.isEmpty || v2.isEmpty || v1.length != v2.length) throw new Exception("invalid input")
    v1.indices.map(i => v1(i) * v2(i)).sum
  }

  def calVectorCosine(v1: Seq[Double], v2: Seq[Double]) = {
    val cos = innerProduct(v1, v2) / (module(v1) * module(v2))
    if (cos <= 1) cos else 1.0
  }

  def calMatrixCosine(m1: Seq[Seq[Double]], m2: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    if (m1.isEmpty || m2.isEmpty || m1.length != m2.length) throw new Exception("invalid input")
    m1.indices.map(u => m2.indices.map(v => calVectorCosine(m1(u), m2(v))))
  }

  def main(args: Array[String]): Unit = {
    val matrix = Seq(Seq(1.0, 2.0), Seq(-1.0, -2.0))
    val t = calMatrixCosine(matrix, matrix)
    println(t(0)(1))
  }

}
