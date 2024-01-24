#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define SIZE 250 // Maximum size/width of a spiral

char spiral[SIZE][SIZE], movespiral[SIZE][SIZE]; // Initialize 2D arrays of commands and movement directions

int codex = 0, posx = 0, posy = 0, direction = 6, times = 0, offset = 0, edge = 0, jumping = 0;


void rotate(int amount) { // Rotate IP direction clockwise
  times+=amount;
  if (direction == 6){direction = 8;}
  else if (direction == 8){direction = 4;}
  else if (direction == 4){direction = 2;}
  else if (direction == 2){direction = 6;}
}

void move(int amount) { // Move IP
  if (direction == 6){posx += amount;}
  else if (direction == 8){posy += amount;}
  else if (direction == 4){posx -= amount;}
  else if (direction == 2){posy -= amount;}
}

void jumpin(){ // Make IP jump inwards
  jumping = 1;
  rotate(0);
  move(1);
  if (movespiral[posy][posx] != 0){ // Check if program tries to jump inwards on a corner
    printf("\nERROR: ATTEMPT TO JUMP INWARDS WHILE ON CORNER."); exit(0);
  }
  while (movespiral[posy][posx] == 0){
    move(1);
    if (posx < 0){ // Check if IP is out of bounds
      printf("\nERROR: THE IP FLEW AWAY, NEVER TO BE SEEN AGAIN."); exit(0);
    }
  }
  direction = movespiral[posy][posx];
}

void jumpout(){ // Make IP jump outwards
  jumping = 1;
  rotate(0);rotate(0);rotate(0);
  move(1);
  while (movespiral[posy][posx] == 0){move(1);}
  direction = movespiral[posy][posx];
}

int printmode = 0, back = 0;

long long accumulator = 0, queue[2000], qtwo[2000]; // Initialize accumulator and queue

void enqueue(long long toq) { // Enqueue value
  back++;
  if (back == 2000){ // Check if queue is full
    printf("\nERROR: QUEUE OVERFLOW.");
    exit(0);
  }
  queue[back] = toq;
}

void printqueue() { // Output entire queue
  int amount = 0;
  printf("[ ");
  while (amount <= back) {
    printf("%lld",queue[amount]);
    putchar(32);
    amount++;
  }
  printf("]\n");
}

void dequeue() { // Dequeue from queue
  for (int i=0; i < back; i++) {queue[i] = queue[i+1];}
  back--;
}

void reverse() { // Reverse queue
  int down = back;
  int up = 0;
  while (down >=0) {
      qtwo[up] = queue[down];
      up++;
      down--;
  }
  up = 0;
  while (up <= back) {
    queue[up] = qtwo[up];
    up++;
  }
   
}

void bigjumpout() { // Jump outwards to first layer of spiral
  jumping = 1;
  if (direction == 2) {
    posx = 0;
  }
  if (direction == 8) {
    posx = (edge-2);
  }
  if (direction == 4) {
    posy = (edge-2);
  }
  if (direction == 6) {
    posy = 0;
  }
}

void interpret() { // Interpret a command on the spiral
  char t = spiral[posy][posx];
  if (t == '#') {printmode = 1;}
  else if (t == 'B') {jumpin();}
  else if (t == 'H') {jumpout();}
  else if (t == 'E') {printqueue();}
  else if (t == 'Q') {enqueue(0);}
  else if (t == 'Z') {enqueue(queue[0]);}
  else if (t == 'X') {dequeue();}
  else if (t == 'I') {queue[0]++;}
  else if (t == 'D') {queue[0]--;}
  else if (t == 'O') {printf("%lld",queue[0]);}
  else if (t == 'R') {reverse();}
  else if (t == 'A') {accumulator = queue[0];}
  else if (t == 'C') {putchar(queue[0]);}
  else if (t == 'V') {queue[0] += accumulator;}
  else if (t == 'P') {queue[0] += 10;}
  else if (t == 'M') {queue[0] -= 10;}
  else if (t == 'W') {bigjumpout();}
  else if (t == 'G') {long long input;scanf("%lld",&input);enqueue(input);}
  else if (t == 'S') {char input;if (scanf("%c",&input) != EOF){enqueue(input);}
    else {enqueue(0);}}
  else if (t == 'J') {if (queue[0] == accumulator){move(1);
      if (movespiral[posy][posx] != 9) {direction = movespiral[posy][posx];}}}
  else if (t == 'K') {if (queue[0] != accumulator) {move(1);
      if (movespiral[posy][posx] != 9) {direction = movespiral[posy][posx];}}}
  else {printf("\nERROR: UNRECOGNIZED COMMAND '"); putchar(t); printf("'."); exit(0);}
  while (printmode == 1) {
    direction = movespiral[posy][posx];
    move(1);
    if (spiral[posy][posx] != '#') {
      putchar(spiral[posy][posx]);
    }
    if (spiral[posy][posx] == '#') {
      printmode = 0;
    }
    direction = movespiral[posy][posx];
  }
}

void main(int argc, char **argv) {

if (argc < 2) // Get source code from file
    puts("ERROR: NO FILE GIVEN."), exit(0);
FILE *file = fopen(argv[1], "r");
if (file == NULL)
    puts("ERROR: FILE DOES NOT EXIST."), exit(0);
if (fseek(file, 0, SEEK_END) != 0)
    puts("ERROR"), exit(0);
int posn = ftell(file);
if (posn == -1)
    puts("ERROR"), exit(0);
rewind(file);
char *fileconts = malloc(posn);
if (fileconts == NULL)
    puts("ERROR"), exit(0);
for (int i = 0, c; EOF != (c = fgetc(file)); fileconts[i] = c, i++);

fclose(file);

while (fileconts[codex] != 0){ // Fill spiral array with source code
  if (fileconts[codex] == '\n'){
    if (posy == 0){
      edge = codex+1;
      if (edge > SIZE+1) { // Check if spiral exceeds maximum size
        printf("\nERROR: SPIRAL TOO BIG TO INTERPRET."); exit(0);
      }
    }
    posy++;
    posx = 0;
  }
  else{
    spiral[posy][posx] = fileconts[codex];
    posx++;
  }
  codex++;
}

free(fileconts);

posx = 0; posy = 0;

while (times < edge-1){ // Fill movement array with movement directions
  if (posx==offset && posy==offset+2){rotate(1);offset+=2;}
  else if (posx==(edge-2)-offset && posy==offset){rotate(1);}
  else if (posx==offset && posy==(edge-2)-offset){rotate(1);}
  else if (posx==(edge-2)-offset && posy==(edge-2)-offset){rotate(1);}
  movespiral[posy][posx] = direction; move(1);
}

move(-1);
movespiral[posy][posx] = 9;
move(1);

posy = 0; posx = 0;

while (1) { // Main loop
  if (movespiral[posy][posx] != 9){direction = movespiral[posy][posx];}
  jumping = 0;
  interpret();
  if (movespiral[posy][posx] == 9){break;}
  if (jumping == 0) {move(1);}
}
}
