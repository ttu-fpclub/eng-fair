/*
 * This file MUST be compiled in at least C++11. GCC
 * requires the special flag --std=c++0x to do so.
 */

#include <utility>
#include <iostream>
#include <set>

using namespace std;

template <typename T>
void quicksort(T* arr, int begin, int end) {
    int n = begin;
    int pivot = end - 1;
    if (end - begin > 1) {
        for (int i = begin + 1; i < end; i++) {
            if (arr[i] < arr[pivot]) {
                swap(arr[i], arr[n]);
                n++;
            }
        }
        swap(arr[pivot], arr[n]);
        quicksort(arr, begin, n);
        quicksort(arr, n + 1, end);
    }
}

template <typename T>
void quicksort(T* arr, int length) {
    quicksort(arr, 0, length);
}

bool isPrime(int n) {
    for (int i = 2; i < n; i++) {
        if (n % i == 0)
            return false;
    }
    return true;
}

class Primes {
private:
    int curr;
public:
    Primes() : curr(2) {}
    int get() {return curr;}
    void next() {
        ++curr;
        while (!isPrime(curr))
            ++curr;
    }
};

int gcd(int n, int m) {
    while (m != 0) {
        int r = n % m;
        n = m;
        m = r;
    }
    return n;
}

template <typename Set>
set<Set> powerset(const Set& st, size_t len) {
    set<Set> res;
    if (len > 0) {
        set<Set> ps = powerset(st, len - 1);
        for (auto& ss : ps) {
            for (auto& el : st) {
                Set subset(ss);
                subset.insert(el);
                res.insert(subset);
            }
        }
        res.insert(ps.begin(), ps.end());
    } else {
        res.insert(Set());
    }
    return res;
}

int main(int argc, char** argv) {
    int arr[] = {3, 5, 4, 2, 1};
    quicksort(arr, 5);
    for (int i = 0; i < 5; i++) {
        cout << arr[i] << endl;
    }
    cout << endl;
    cout << boolalpha << "4 " << isPrime(4) << endl << "5 " << isPrime(5) << endl;
    cout << endl;
    Primes primes;
    for (int i = 0; i < 10; i++) {
        cout << primes.get() << ", ";
        primes.next();
    }
    cout << "..." << endl << endl;
    cout << "gcd(10, 15) = " << gcd(10, 15) << endl;
    cout << "gcd(45, 33) = " << gcd(45, 33) << endl;
    cout << endl;
    set<int> ss{1, 2, 3};
    auto ps = powerset(ss, ss.size());
    for (auto s : ps) {
        cout << "{";
        for (auto x : s) {
            cout << x << ", ";
        }
        cout << "}";
    }
    cout << endl << endl;
}
