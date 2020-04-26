#include <iostream>
#include <optional>
#include <random>
#include <string>
#include <vector>

using namespace std;

enum class OpType { Unknown, Plus, Minus, Multiply, Divide };

struct Number {
  float value;
  string expr;
  OpType op;
  Number() {}
  Number(const float v, const string& e, const OpType o) : value(v), expr(e), op(o) {}
  Number(const Number& num1, const Number& num2, const OpType current_op) {
    op = current_op;
    switch (op) {
    case OpType::Plus:
      value = num1.value + num2.value;
      expr.append(num1.expr).append(" + ").append(num2.expr);
      break;
    case OpType::Minus:
      value = num1.value - num2.value;
      expr.append(num1.expr).append(" - ").append(create_expr(num2, false));
      break;
    case OpType::Multiply:
      value = num1.value * num2.value;
      expr.append(create_expr(num1, false)).append(" * ").append(create_expr(num2, false));
      break;
    case OpType::Divide:
      value = num1.value / num2.value;
      expr.append(create_expr(num1, false)).append(" / ").append(create_expr(num2, true));
      break;
    default:
      exit(1);
    }
  }

  inline string create_expr(const Number &num, const bool is_denominator) {
    std::string s;
    if (num.op == OpType::Plus || num.op == OpType::Minus ||
        (is_denominator && num.op != OpType::Unknown))
      return s.append("(").append(num.expr).append(")");
    return num.expr;
  }
};

vector<Number> combine(const Number& num1, const Number& num2) {
  vector<Number> result;
  result.emplace_back(num1, num2, OpType::Plus);
  result.emplace_back(num1, num2, OpType::Multiply);
  if (num1.value > num2.value) {
    result.emplace_back(num1, num2, OpType::Minus);
  } else {
    result.emplace_back(num2, num1, OpType::Minus);
  }
  if (num2.value != 0) {
    result.emplace_back(num1, num2, OpType::Divide);
  }
  if (num1.value != 0) {
    result.emplace_back(num2, num1, OpType::Divide);
  }
  return result;
}

optional<string> calc(const vector<Number>& nums, const float target) {
  if (nums.size() == 1) {
    if (nums[0].value == target) {
      return nums[0].expr;
    } else {
      return {};
    }
  }

  vector<Number> reduced;
  for (int i = 0; i < nums.size(); i++) {
    for (int j = i + 1; j < nums.size(); j++) {
      reduced.clear();
      for (int k = 0; k < nums.size(); k++) {
        if (k == i || k == j) continue;
        reduced.push_back(nums[k]);
      }
      for (const Number& num : combine(nums[i], nums[j])) {
        reduced.push_back(num);
        const auto result = calc(reduced, target);
        if (result.has_value()) {
          return result;
        }
        reduced.pop_back();
      }
    }
  }
  return {};
}

string calc24(vector<int> nums) {
  vector<Number> numbers;
  string challenge;
  for (int i = 0; i < nums.size(); i++) {
    int x = nums[i];
    numbers.emplace_back(float(x), to_string(x), OpType::Unknown);
    if (i > 0)
      challenge.append(", ");
    challenge.append(numbers[i].expr);
  }
  challenge.append(" -> ");
  const auto result = calc(numbers, 24);
  if (result.has_value()) {
    challenge.append(result.value());
  } else {
    challenge.append("No Solution");
  }
  return challenge;
}

int main() {
  srand(time(NULL));
  vector<int> nums(4);
  for (int i = 0; i < 1000; i++) {
    nums.clear();
    for (int j = 0; j < 4; j++) {
      nums.push_back(rand() % 13 + 1);
    }
    cout << calc24(nums) << endl;
  }
  return 0;
}
