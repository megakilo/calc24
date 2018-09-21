#include <algorithm>
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <vector>

using namespace std;

struct Number {
  float value;
  string expr;
  char op;
  Number() {}
  Number(float v, string e, char o) : value(v), expr(e), op(o) {}
};

using NumberPtr = shared_ptr<Number>;

string add_pr(const NumberPtr &num, string conditions) {
  if (conditions.find(num->op) != string::npos) {
    return "(" + num->expr + ")";
  } else {
    return num->expr;
  }
}

vector<NumberPtr> combine(const NumberPtr& num1, const NumberPtr& num2) {
  vector<NumberPtr> result;
  result.push_back(make_shared<Number>(
    num1->value + num2->value, num1->expr + " + " + num2->expr, '+'));
  result.push_back(make_shared<Number>(
    num1->value * num2->value, add_pr(num1, "+-") + " * " + add_pr(num2, "+-"), '*'));
  if (num1->value > num2->value) {
    result.push_back(make_shared<Number>(
      num1->value - num2->value, num1->expr + " - " + add_pr(num2, "+-"), '-'));
  } else {
    result.push_back(make_shared<Number>(
      num2->value - num1->value, num2->expr + " - " + add_pr(num1, "+-"), '-'));
  }
  if (num2->value != 0) {
    result.push_back(make_shared<Number>(
      num1->value / num2->value, add_pr(num1, "+-") + " / " + add_pr(num2, "+-*"), '/'));
  }
  if (num1->value != 0) {
    result.push_back(make_shared<Number>(
      num2->value / num1->value, add_pr(num2, "+-") + " / " + add_pr(num1, "+-*"), '/'));
  }
  return result;
}

string calc(const vector<NumberPtr>& nums, const float target) {
  if (nums.size() == 1) {
    if (nums[0]->value == target) {
      return nums[0]->expr;
    } else {
      return "";
    }
  }

  for (int i = 0; i < nums.size(); i++) {
    for (int j = i + 1; j < nums.size(); j++) {
      vector<NumberPtr> reduced;
      for (int k = 0; k < nums.size(); k++) {
        if (k == i || k == j) continue;
        reduced.push_back(nums[k]);
      }
      for (const NumberPtr& num : combine(nums[i], nums[j])) {
        reduced.push_back(num);
        string result = calc(reduced, target);
        if (result != "") {
          return result;
        }
        reduced.pop_back();
      }
    }
  }
  return "";
}

int main() {
  srand(time(NULL));
  vector<NumberPtr> nums(4);
  for (int i = 0; i < 1000; i++) {
    for (int j = 0; j < 4; j++) {
      int t = rand() % 13 + 1;
      nums[j] = make_shared<Number>(float(t), to_string(t), 'x');
    }
    string challenge = accumulate(
        nums.begin(), nums.end(), string(), [](string& ss, NumberPtr& p) {
          return ss.empty() ? p->expr : ss + ", " + p->expr;
        });
    string result = calc(nums, 24);
    if (result != "") {
      cout << challenge << " -> " << result << endl;
    } else {
      cout << challenge << " -> No Solution" << endl;
    }
  }
  return 0;
}