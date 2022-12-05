#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
  Rock = 'X',
  Paper = 'Y',
  Scissors = 'Z',

  Loss = 'X',
  Tie = 'Y',
  Win = 'Z',
};

long long score_game(int opp, int me) {
  if ((opp == Rock && me == Paper) || (opp == Scissors && me == Rock) || (opp == Paper && me == Scissors)) {
    return 6;
  } else if ((opp == Rock && me == Scissors) || (opp == Scissors && me == Paper) || (opp == Paper && me == Rock)) {
    return 0;
  }  else {
    return 3;
  }
}

int get_play(int opp, int result) {
  if ((opp == Rock && result == Loss) || (opp == Paper && result == Win)) {
    return Scissors;
  } else if ((opp == Scissors && result == Loss) || (opp == Rock && result == Win)) {
    return Paper;
  } else if ((opp == Paper && result == Loss) || (opp == Scissors && result == Win)) {
    return Rock;
  } else {
    return opp;
  }
}

void part_one(char const* input, long length) {
  long long score = 0;
  
  for(int x = 0; x < length; x += 4) {
    char opp  = input[x] + 23;
    char me   = input[x + 2];

    score += score_game(opp, me);

    switch (me) {
      case Rock:     score += 1; break;
      case Paper:    score += 2; break;           
      case Scissors: score += 3; break;            
    }
  }
  
  printf("Score for part 1: %lld\n", score);
} 

void part_two(char const* input, long length) {
  long long score = 0;
  
  for(int x = 0; x < length; x += 4) {
    char opp    = input[x] + 23;
    char result = input[x + 2];
    char me     = get_play(opp, result);

    score += score_game(opp, me);

    switch (me) {
      case Rock:     score += 1; break;
      case Paper:    score += 2; break;           
      case Scissors: score += 3; break;            
    }
  }
  
  printf("Score for part 1: %lld\n", score);
}

int main(int argc, char* argv[]) {
  FILE* file = fopen("./input/day02.txt", "r");
  if (file == NULL) {
    fprintf(stderr, "Unable to open file...");
    return 1;
  }
  
  fseek(file, 0, SEEK_END);
  long length = ftell(file);
  fseek(file, 0, SEEK_SET);
  
  char* string = malloc(length + 1);
  if (string == NULL) {
    fprintf(stderr, "Malloc failed... ");
    return 1;
  } 
  
  fread(string, 1, length, file);
  string[length] = '\n';

  fclose(file);
     
  part_one(string, length);
  part_two(string, length);
}
