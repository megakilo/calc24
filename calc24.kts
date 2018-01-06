import java.util.*

typealias Number = Pair<Float, String>

fun <T> split(nums: List<T>, n: Int): List<Pair<List<T>, List<T>>> {
    if (n == 0) return listOf(Pair(emptyList(), nums))
    if (nums.size <= n) return listOf(Pair(nums, emptyList()))
    val head = nums.first()
    val tail = nums.drop(1)
    return split(tail, n).map { pair -> Pair(pair.first, pair.second + head) } +
            split(tail, n-1).map { pair -> Pair(pair.first + head, pair.second) }
}

fun combine2(nums: List<Number>): List<Number> = listOf(
        Number(nums[0].first + nums[1].first, "(${nums[0].second} + ${nums[1].second})"),
        Number(nums[0].first * nums[1].first, "(${nums[0].second} * ${nums[1].second})")) +
        (if (nums[0].first > nums[1].first)
            listOf(Number(nums[0].first - nums[1].first, "(${nums[0].second} - ${nums[1].second})"))
        else
            listOf(Number(nums[1].first - nums[0].first, "(${nums[1].second} - ${nums[0].second})"))) +
        (if (nums[1].first != 0f)
            listOf(Number(nums[0].first / nums[1].first, "(${nums[0].second} / ${nums[1].second})"))
        else emptyList()) +
        (if (nums[0].first != 0f)
            listOf(Number(nums[1].first / nums[0].first, "(${nums[1].second} / ${nums[0].second})"))
        else emptyList())

fun reduce(nums: List<Number>): Sequence<List<Number>> = split(nums, 2).asSequence().flatMap { pair ->
    combine2(pair.first).map { i -> pair.second + i }.asSequence()
}

fun calc(nums: List<Number>): Sequence<Number> {
    if (nums.size == 1) return nums.asSequence()
    return reduce(nums).flatMap { xs -> calc(xs) }
}

val rand = Random()
(1..100).forEach {
    val numbers = (1..4).map{ rand.nextInt(13) + 1}
    val result = calc(numbers.map{Number(it.toFloat(), it.toString())}).find{it.first == 24f}?.second ?: "No Solution"
    println("numbers: $numbers; solution: $result")
}
