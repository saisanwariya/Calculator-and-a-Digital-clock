# Calculator and Digital Clock Program with HCS12


## Academic Integrity Statement

Please note that all work included in this project is the original work of the author, and any external sources or references have been properly cited and credited. It is strictly prohibited to copy, reproduce, or use any part of this work without permission from the author.

If you choose to use any part of this work as a reference or resource, you are responsible for ensuring that you do not plagiarize or violate any academic integrity policies or guidelines. The author of this work cannot be held liable for any legal or academic consequences resulting from the misuse or misappropriation of this work.

Any unauthorized copying or use of this work may result in serious consequences, including but not limited to academic penalties, legal action, and damage to personal and professional reputation. Therefore, please use this work only as a reference and always ensure that you properly cite and attribute any sources or references used.

## Overview

This project combines the functionality of Calculator and Digital clock to create a program for an HCS12 board. The program implements both a calculator and a digital clock that run simultaneously. The calculator allows users to perform mathematical operations, while the digital clock displays the current time. The calculator output is shown on the terminal screen, and the time is displayed using a 7-segment display connected to PORTA and PORTB of the HCS12 board.

## Program Functionality

1. **Calculator Rules**:
   - It displays a 'Tcalc> ' prompt and echoes user keystrokes until the Return key is pressed.
   - Valid mathematical operations include addition (+), subtraction (-), multiplication (*), and division (/).
   - The calculator provides error messages for invalid input formats, overflow errors, and division by zero.
   
2. **Digital Clock Rules**:
   - It updates the time display every second.
   - The time is displayed using a 7-segment display.
   - The Real Time Interrupt feature is used to keep track of time.

## Hardware Setup

Ensure that you have the following hardware setup:
- HCS12 board.
- Terminal connected to the HCS12 board for calculator output.
- 7-segment display connected to PORTA and PORTB for digital clock display.

## Software Setup

To set up the software environment, follow these steps:

1. Open the CodeWarrior software.
2. Create a new project with the program starting at address $3100 and data at address $3000.

## Running the Program

To run the program on the HCS12 board, follow these steps:

1. Build the project in CodeWarrior to generate the binary file.
2. Download the binary file onto the HCS12 board.
3. Connect the terminal to the HCS12 board for calculator input and output.
4. Ensure that the 7-segment display is connected to PORTA and PORTB.
5. Power on the HCS12 board.

The program will start, displaying 'Tcalc> ' for calculator input. Follow the provided user directions to use the calculator and set the time on the digital clock. The program is designed to be user-friendly and robust, so it should not crash or stop based on incorrect user responses.

## Notes

- Make sure to refer to the MC9S12C128 Family Data Sheet for relevant information on the HCS12 board.
- The program design should start at $3100 for code and $3000 for data.
- Ensure that the Real Time Interrupt feature is utilized for keeping track of time in the digital clock.
