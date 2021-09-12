hard_maze:
    .byte   %11111111,%11111111,%11111111,%11111111 ; blank rows for score, etc.
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %11111111,%11111111,%11111111,%11111111 ; top wall
    .byte   %10000000,%01000000,%00100000,%00000001 ; p1 - top path
    .byte   %11111101,%11110111,%11111111,%11111101 ; w1
    .byte   %10000100,%00000100,%00000000,%00010001 ; p2
    .byte   %10111110,%11111111,%11011111,%11111101 ; w2
    .byte   %10000000,%00000000,%10000000,%00000000 ; p3 - exit
    .byte   %10111111,%11011111,%11110111,%11111111 ; w3
    .byte   %10000000,%10000000,%00100010,%00000001 ; p4
    .byte   %11111101,%11111111,%11101111,%11111101 ; w4
    .byte   %10001000,%00001000,%00000000,%00010001 ; p5
    .byte   %10111111,%10111111,%01111111,%11111101 ; w5
    .byte   %10000000,%00000000,%00010000,%00000001 ; p6
    .byte   %10111111,%01111111,%11111011,%11111101 ; w6
    .byte   %10000010,%00000000,%10000000,%00010001 ; p7
    .byte   %11111111,%11111011,%11111111,%11111101 ; w7
    .byte   %10000000,%10000000,%00000010,%00000001 ; p8
    .byte   %10111111,%11111111,%11110111,%11110111 ; w8
    .byte   %10000000,%00000000,%00100000,%00000001 ; p9
    .byte   %10111111,%11110111,%11101111,%11111101 ; w9
    .byte   %10000000,%00100001,%00000001,%00000001 ; p10
    .byte   %10111110,%11111111,%11111111,%11111101 ; w10
    .byte   %10001000,%00000000,%00100000,%00000001 ; p11
    .byte   %11111111,%11110111,%11111111,%11110111 ; w11
    .byte   %10000000,%10000000,%00000000,%01000001 ; p12
    .byte   %10111111,%11111111,%11101111,%11111101 ; w12
    .byte   %10000000,%00000000,%00000000,%00000001 ; p13 - bottom path
    .byte   %11111111,%11111111,%11111111,%11111111 ; bottom wall
    .byte   %11111111,%11111111,%11111111,%11111111

maze_template:
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %11111111,%11111111,%11111111,%11111111 ; top wall
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111
    .byte   %10101010,%10101010,%10101010,%10101011
    .byte   %11111111,%11111111,%11111111,%11111111 ; bottom wall
    .byte   %11111111,%11111111,%11111111,%11111111
