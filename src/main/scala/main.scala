def calculateMinimumHP(dungeon: Array[Array[Int]]): Int =
  val m = dungeon.length
  val n = dungeon(0).length

  // dp(i)(j): the minimum health needed to reach the princess from cell (i, j)
  val dp = Array.ofDim[Int](m + 1, n + 1)

  // Fill dp with a large number
  for
    i <- 0 to m
    j <- 0 to n
  do
    dp(i)(j) = Int.MaxValue

  // The knight needs at least 1 health when he reaches the princess
  dp(m)(n - 1) = 1
  dp(m - 1)(n) = 1

  for
    i <- (0 until m).reverse
    j <- (0 until n).reverse
  do
    val minHealthOnExit = dp(i + 1)(j).min(dp(i)(j + 1))
    dp(i)(j) = (minHealthOnExit - dungeon(i)(j)).max(1)

  dp(0)(0)

@main def runDungeonGameExample(): Unit =
  val dungeon = Array(
    Array(-2, -3, 3),
    Array(-5, -10, 1),
    Array(10, 30, -5)
  )

  val minInitialHealth = calculateMinimumHP(dungeon)
  println(s"Minimum initial health required: $minInitialHealth")