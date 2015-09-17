#include <iostream>
#include <ctime>
#include <cstdlib>

using namespace std;

int main(int argc, char** argv) {
    srand(time(nullptr));
    int corr = rand() % 1000 + 1;
    int guesses = 12;
    bool done = false;
    while ((!done) && (guesses > 0)) {
        cout << "Guess a number: " << endl;
        int guess;
        cin >> guess;
        if ((guess > 0) && (guess <= 1000)) {
            if (guess > corr) {
                cout << "Too high..." << endl;
            } else if (guess < corr) {
                cout << "Too low..." << endl;
            } else {
                cout << "You got it!" << endl;
                done = true;
            }
            guesses--;
        }
    }
    if (!done)
        cout << "You lose... it was " << corr << endl;
}
