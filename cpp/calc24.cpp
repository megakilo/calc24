#include <iostream>
#include <memory>
#include <optional>
#include <random>
#include <string>
#include <vector>

enum class OpType { None, Plus, Minus, Multiply, Divide };

struct Number {
  float value;
  Number* left;
  Number* right;
  OpType op;
  explicit Number(const float v) : value(v), op(OpType::None) {}
  Number(Number* num1, Number* num2,
         const OpType current_op) {
    op = current_op;
    left = num1;
    right = num2;
    switch (op) {
      case OpType::None:
        value = 0;
        break;
      case OpType::Plus:
        value = num1->value + num2->value;
        break;
      case OpType::Minus:
        value = num1->value - num2->value;
        break;
      case OpType::Multiply:
        value = num1->value * num2->value;
        break;
      case OpType::Divide:
        value = num1->value / num2->value;
        break;
    }
  }
};

std::string print(Number* num);

std::string create_expr(Number* num,
                        const bool is_denominator) {
  std::string s;
  if (num->op == OpType::Plus || num->op == OpType::Minus ||
      (is_denominator && num->op != OpType::None))
    return s.append("(").append(print(num)).append(")");
  return s.append(print(num));
}

std::string print(Number* num) {
  std::string s;
  switch (num->op) {
    case OpType::None:
      s.append(std::to_string(int(num->value)));
      break;
    case OpType::Plus:
      s.append(print(num->left)).append(" + ").append(print(num->right));
      break;
    case OpType::Minus:
      s.append(print(num->left))
          .append(" - ")
          .append(create_expr(num->right, false));
      break;
    case OpType::Multiply:
      s.append(create_expr(num->left, false))
          .append(" * ")
          .append(create_expr(num->right, false));
      break;
    case OpType::Divide:
      s.append(create_expr(num->left, false))
          .append(" / ")
          .append(create_expr(num->right, true));
      break;
  }
  return s;
}

std::vector<std::unique_ptr<Number>> combine(Number* num1, Number* num2) {
  std::vector<std::unique_ptr<Number>> result;
  result.push_back(std::make_unique<Number>(num1, num2, OpType::Plus));
  result.push_back(std::make_unique<Number>(num1, num2, OpType::Multiply));
  if (num1->value > num2->value) {
    result.push_back(std::make_unique<Number>(num1, num2, OpType::Minus));
  } else {
    result.push_back(std::make_unique<Number>(num2, num1, OpType::Minus));
  }
  if (num2->value != 0) {
    result.push_back(std::make_unique<Number>(num1, num2, OpType::Divide));
  }
  if (num1->value != 0) {
    result.push_back(std::make_unique<Number>(num2, num1, OpType::Divide));
  }
  return result;
}

std::optional<std::string> calc(const std::vector<Number*>& nums, const float target) {
  const int N = nums.size();
  if (N == 1) {
    if (nums[0]->value == target) {
      return print(nums[0]);
    } else {
      return {};
    }
  }

  std::vector<Number*> reduced;
  reduced.reserve(N-1);
  for (int i = 0; i < N; i++) {
    for (int j = i + 1; j < N; j++) {
      reduced.clear();
      for (int k = 0; k < N; k++) {
        if (k == i || k == j) continue;
        reduced.push_back(nums[k]);
      }
      reduced.push_back(nullptr);
      for (const auto& num : combine(nums[i], nums[j])) {
        reduced[N-2] = num.get();
        const auto result = calc(reduced, target);
        if (result.has_value()) {
          return result;
        }
      }
    }
  }
  return {};
}

std::string calc24(const std::vector<int>& nums) {
  std::vector<std::unique_ptr<Number>> numbers;
  std::vector<Number*> num_ptrs;
  std::string challenge;
  for (int i = 0; i < nums.size(); i++) {
    int x = nums[i];
    numbers.emplace_back(std::make_unique<Number>(float(x)));
    num_ptrs.push_back(numbers[i].get());
    if (i > 0) challenge.append(", ");
    challenge.append(std::to_string(x));
  }
  challenge.append(" -> ");
  const auto result = calc(num_ptrs, 24);
  if (result.has_value()) {
    challenge.append(result.value());
  } else {
    challenge.append("No Solution");
  }
  return challenge;
}

int main() {
  srand(time(NULL));
  std::vector<int> nums;
  for (int i = 0; i < 1000; i++) {
    nums.clear();
    for (int j = 0; j < 4; j++) {
      nums.push_back(rand() % 13 + 1);
    }
    std::cout << calc24(nums) << std::endl;
  }
  return 0;
}
