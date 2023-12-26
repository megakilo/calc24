#include <array>
#include <iostream>
#include <memory>
#include <optional>
#include <random>
#include <string>
#include <vector>

enum class OpType { None, Add, Subtract, Multiply, Divide };

struct Number {
  int index = -1;
  std::shared_ptr<Number> left = nullptr;
  std::shared_ptr<Number> right = nullptr;
  OpType op = OpType::None;

  explicit Number(const int i) : index(i), op(OpType::None) {}
  Number(std::shared_ptr<Number> num1, std::shared_ptr<Number> num2,
         const OpType current_op)
      : left(num1), right(num2), op(current_op) {}

  template <typename std::size_t N>
  double eval(const std::array<double, N>& a) {
    switch (op) {
      case OpType::None:
        return a[index];
      case OpType::Add:
        return left->eval(a) + right->eval(a);
      case OpType::Subtract:
        return left->eval(a) - right->eval(a);
      case OpType::Multiply:
        return left->eval(a) * right->eval(a);
      case OpType::Divide:
        return left->eval(a) / right->eval(a);
    }
  }

  template <typename std::size_t N>
  std::string print(const std::array<double, N>& a, const bool is_denominator) {
    std::string s;
    if (op == OpType::Add || op == OpType::Subtract ||
        (op != OpType::None && is_denominator)) {
      return s.append("(").append(print(a)).append(")");
    } else {
      return s.append(print(a));
    }
  }

  template <typename std::size_t N>
  std::string print(const std::array<double, N>& a) {
    std::string s;
    switch (op) {
      case OpType::None:
        return std::to_string(int(a[index]));
      case OpType::Add:
        return s.append(left->print(a)).append(" + ").append(right->print(a));
      case OpType::Subtract:
        return s.append(left->print(a))
            .append(" - ")
            .append(right->print(a, false));
      case OpType::Multiply:
        return s.append(left->print(a, false))
            .append(" * ")
            .append(right->print(a, false));
      case OpType::Divide:
        return s.append(left->print(a, false))
            .append(" / ")
            .append(right->print(a, true));
    }
  }
};

std::vector<std::shared_ptr<Number>> Combine(std::shared_ptr<Number> num1,
                                             std::shared_ptr<Number> num2) {
  std::vector<std::shared_ptr<Number>> result;
  result.push_back(std::make_shared<Number>(num1, num2, OpType::Add));
  result.push_back(std::make_shared<Number>(num1, num2, OpType::Subtract));
  result.push_back(std::make_shared<Number>(num2, num1, OpType::Subtract));
  result.push_back(std::make_shared<Number>(num1, num2, OpType::Multiply));
  result.push_back(std::make_shared<Number>(num1, num2, OpType::Divide));
  result.push_back(std::make_shared<Number>(num2, num1, OpType::Divide));
  return result;
}

std::vector<std::shared_ptr<Number>> GenerateExpressions(
    const std::vector<std::shared_ptr<Number>>& nums) {
  const int N = nums.size();
  if (N == 1) {
    return nums;
  }
  std::vector<std::shared_ptr<Number>> result;
  std::vector<std::shared_ptr<Number>> reduced;
  for (int i = 0; i < N; i++) {
    for (int j = i + 1; j < N; j++) {
      reduced.clear();
      for (int k = 0; k < N; k++) {
        if (k == i || k == j) continue;
        reduced.push_back(nums[k]);
      }
      reduced.push_back(nullptr);
      for (const auto& num : Combine(nums[i], nums[j])) {
        reduced[N - 2] = num;
        auto r = GenerateExpressions(reduced);
        result.insert(result.end(), r.begin(), r.end());
      }
    }
  }
  return result;
}

std::vector<std::shared_ptr<Number>> GenerateExpressions(int N) {
  std::vector<std::shared_ptr<Number>> init;
  for (int i = 0; i < N; ++i) {
    init.push_back(std::make_shared<Number>(i));
  }
  return GenerateExpressions(init);
}

template <std::size_t N>
std::optional<std::string> calc24(
    const std::vector<std::shared_ptr<Number>>& all_expr,
    const std::array<double, N>& nums) {
  for (const auto& e : all_expr) {
    if (e->eval(nums) == 24) {
      return e->print(nums);
    }
  }
  return {};
}

int main() {
  srand(time(NULL));

  constexpr int N = 4;
  auto all_calculations = GenerateExpressions(N);

  std::array<double, N> nums;
  for (int i = 0; i < 100000; i++) {
    std::string challenge;
    for (int j = 0; j < N; j++) {
      int x = rand() % 13 + 1;
      nums[j] = x;
      if (j > 0) challenge.append(", ");
      challenge.append(std::to_string(x));
    }
    challenge.append(" -> ");
    const auto result = calc24(all_calculations, nums);
    if (result.has_value()) {
      challenge.append(result.value());
    } else {
      challenge.append("No Solution");
    }
    std::cout << challenge << std::endl;
  }
  return 0;
}
