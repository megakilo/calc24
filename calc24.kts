fun comb(nums: List<Int>, n: Int): List<List<Int>> {
    if (n == 0) return listOf(emptyList())
    if (nums.size <= n) return listOf(nums)
    val tail = nums.subList(1, nums.size)
    return comb(tail, n) + comb(tail, n-1).map { xs -> xs + nums[0] }
}

fun combine2(nums: List<Float>): List<Float> {
    return listOf(nums[0] + nums[1], nums[0] * nums[1], nums[0] - nums[1], nums[1] - nums[0]) +
            (if (nums[1] != 0f) listOf(nums[0] / nums[1]) else emptyList()) +
            (if (nums[0] != 0f) listOf(nums[1] / nums[0]) else emptyList())
}

fun reduce(nums: List<Float>): List<List<Float>> {
    val all = (0 until nums.size).toList()
    return comb(all, 2).flatMap { indexes ->
        val taken = indexes.map { i -> nums[i] }
        val nontaken = (all - indexes).map { i -> nums[i] }
        combine2(taken).map { i -> nontaken + i}
    }
}

fun calc(nums: List<Float>, target: Float): Boolean {
    if (nums.size == 1) return nums[0] == target
    return reduce(nums).any { xs -> calc(xs, target) }
}

val input = listOf(3f,3f,7f,7f)
println(calc(input, 24f))
