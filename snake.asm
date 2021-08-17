; Copyright 2021 ksaweryr
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
; OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

bits 16
org 0x7c00

; MEMORY MAP
; ----------
; SNAKEMEM:[0x00, 0x02]       - food position (2 bytes; x, y)
; SNAKEMEM:[0x02, 0x04]       - snake speed (2 bytes; dx, dy)
; SNAKEMEM:[0x04, 0x06]       - snake length (2 bytes)
; SNAKEMEM:[0x06, 0x08]       - current pseudo random number from LCG (2 bytes)
; SNAKEMEM:[0x08, 0x0d]       - snake length as string (5 bytes)
; SNAKEMEM:[0x0d, 0x0e]       - flag indicating wheter snake is alive
; SNAKEMEM:[0x0d, DATA_LEN]   - positions of snake segments, from tail to head
;                               (head on [DATA_LEN-2, DATA_LEN]) (each 2 bytes; x, y)

; CONSTANTS
; ---------
; Absolute memory addresses
VIDMEM equ 0xb800               ; video memory address as memory segment
SNAKEMEM equ 0x50               ; game's memory address as memory segment
TIMER equ 0x46c                 ; address of system time

; Game data memory addresses (relative to SNAKEMEM)
FOOD_POS equ 0x00               ; address of food position
SNAKE_SPEED equ 0x02            ; address of snake's speed
SNAKE_LEN equ 0x04              ; address of snake's length
LCG_X equ 0x06                  ; address of current pseudo random number
SZ_SNAKE_LEN equ 0x08           ; address of snake's length string representation
ALIVE equ 0x0d
STACK_BOTTOM equ 0x0e

; Other constants
ROW_LENGTH equ 0x50             ; board width
COL_HEIGHT equ 0x18             ; board height
DATA_LEN equ STACK_BOTTOM + (ROW_LENGTH * COL_HEIGHT * 2) + 2
                                ; length of game's data; relative address of the end of data
LCG_A equ 0x61                  ; LGC a value
LCG_C equ 0x59                  ; LGC c value

; CODE
; ----
start:
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov fs, ax                  ; clear segments registers

init_data:
    mov ax, SNAKEMEM
    mov ds, ax
    mov es, ax
    mov ss, ax                  ; data segment, extra segment, stack segment = SNAKEMEM
    mov cx, DATA_LEN
    xor ax, ax
    mov di, 0x00
    .fill:
        stosb
        loop .fill              ; fill game's memory with 0

    mov word [SNAKE_LEN], 1     ; set snake's length
    mov dx, word [fs:TIMER]
    mov word [LCG_X], dx        ; set seed of LGC
    call move_food              ; place food
    mov byte [ALIVE], 1

init_stack:
    mov bp, DATA_LEN            ; the base pointer points to snake's head
    mov sp, bp
    mov ax, 0x280d
    push ax                     ; push snake's initial position (40, 13)

init_video:
    xor ah, ah
    mov al, 0x03
    int 0x10                    ; set video mode: 80x25 chars, 16-color

    mov ax, VIDMEM
    mov es, ax                  ; set es to point at video memory

game_loop:
    read_key:
        mov ah, 0x01
        int 0x16                    ; check buffer status
        jz calculate_new_head_pos
        xor ah, ah
        int 0x16                    ; get keystroke
        cmp al, 'w'
        je handle_input.w
        cmp al, 's'
        je handle_input.s
        cmp al, 'a'
        je handle_input.a
        cmp al, 'd'
        je handle_input.d
        jmp calculate_new_head_pos

    handle_input:
        ; change the direction of snake by maximum 90 deg.
        .w:
            mov dx, word [SNAKE_SPEED]
            cmp dl, 0x01
            je calculate_new_head_pos
            xor dh, dh
            mov dl, -1
            mov word [SNAKE_SPEED], dx
            jmp calculate_new_head_pos
        .s:
            mov dx, word [SNAKE_SPEED]
            cmp dl, -1
            je calculate_new_head_pos
            xor dh, dh
            mov dl, 0x01
            mov word [SNAKE_SPEED], dx
            jmp calculate_new_head_pos
        .a:
            mov dx, word [SNAKE_SPEED]
            cmp dh, 0x01
            je calculate_new_head_pos
            xor dl, dl
            mov dh, -1
            mov word [SNAKE_SPEED], dx
            jmp calculate_new_head_pos
        .d:
            mov dx, word [SNAKE_SPEED]
            cmp dh, -1
            je calculate_new_head_pos
            xor dl, dl
            mov dh, 0x01
            mov word [SNAKE_SPEED], dx

    calculate_new_head_pos:
        mov bx, word [bp - 0x02]    ; head position
        mov cx, word [SNAKE_SPEED]

        .move_head_vertically:
            add bl, cl
            .check_top_border:
                cmp bl, 0x00
                jge .check_bottom_border
                add bl, COL_HEIGHT
                jmp .move_head_horizontally
            .check_bottom_border:
                cmp bl, COL_HEIGHT
                jl .move_head_horizontally
                sub bl, COL_HEIGHT

        .move_head_horizontally:
            add bh, ch
            .check_left_border:
                cmp bh, 0x00
                jge .check_right_border
                add bh, ROW_LENGTH
                jmp move_body
            .check_right_border:
                cmp bh, ROW_LENGTH
                jl move_body
                sub bh, ROW_LENGTH

    move_body:
        mov si, sp
        mov cx, word [SNAKE_LEN]
        dec cx
        test cx, cx
        jz move_head           ; skip if snake's length is 1
        .loop:
            mov dx, word [si + 0x02]
            cmp dx, bx              ; bx holds head position
            jnz .store
            mov byte [ALIVE], 0     ; end the game if head hits snake's body
            .store:
            mov word [si], dx       ; set nth segment new position to (n-1)th
                                    ; segment old position
            add si, 2
        loop .loop

    move_head:
        mov word [bp - 0x02], bx

    eat_food:
        mov cx, word [FOOD_POS]
        cmp cx, bx
        jne clear_board
        inc word [SNAKE_LEN]
        mov si, sp
        mov bx, word [si]
        push bx                 ; add new snake segment
        call move_food          ; relocate food

    clear_board:
        xor di, di
        mov cx, COL_HEIGHT * ROW_LENGTH
        .loop:
            mov al, ' '
            stosb
            mov al, 0x00            ; colour: black
            stosb
            loop .loop

    draw_snake:
        mov si, sp
        mov bl, byte [ALIVE]
        xor bl, 1               ; bl = 0 if alive; else 1
        shl bl, 1
        add bl, 0x02            ; colour: green if alive; else red
        .loop:
            call coords_to_addr
            add si, 0x02
            mov al, 0xfe        ; CCSID 437 character: black square
            stosb
            mov al, bl
            cmp si, bp
            jz .end
            add al, 0x08        ; brighten all segments except the head
            stosb
            jmp .loop
            .end:
            stosb

    draw_food:
        lea si, word [FOOD_POS]
        call coords_to_addr
        mov al, 0xfe            ; CCSID 437 character: black square
        stosb
        mov al, 0x05            ; colour: purple
        stosb

    print_score:
        mov ax, word [SNAKE_LEN]

        ; convert snake length to string
        mov dl, 0x0a
        mov di, SNAKEMEM + SZ_SNAKE_LEN
        mov byte [di], 0x00
        .itoa_loop:
            dec di
            div dl
            add ah, '0'
            mov byte [di], ah
            xor ah, ah
            test al, al
            jnz .itoa_loop
        mov si, di
        mov di, (ROW_LENGTH * (COL_HEIGHT + 1) - 5) * 2
        .loop:
            lodsb
            test al, al
            jz end_if_dead
            stosb
            mov al, 0x03        ; colour: cyan
            stosb
            jmp .loop

    end_if_dead:
        mov al, byte [ALIVE]
        test al, al
        jz end

    delay:
        mov bx, [fs:TIMER]
        add bx, 2               ; wait for 2 ticks
        .wait:
            cmp word [fs:TIMER], bx
            jl .wait

    jmp game_loop

end:
    .await_restart:
        mov ah, 0x01
        int 0x16
        jz .await_restart           ; do nothing until any key is pressed
    jmp start                       ; restart the game


; FUNCTIONS
; ---------

; change (x, y) coordinates to memory address
; param si: address of (x, y) coordinates
; return value: di
; clobbered registres: ax, dx
coords_to_addr:
    mov dx, word [si]
    mov al, ROW_LENGTH
    shl al, 0x01
    mul dl
    movzx di, dh
    shl di, 0x01
    add di, ax
    ret

; move food to next position
; clobbered registers: ax, cx, dx
move_food:
    .next_random_number:
        mov ax, word [LCG_X]
        mov dx, LCG_A
        mul dx
        add ax, LCG_C
        mov word [LCG_X], ax

    mov cx, ax
    movzx ax, al
    mov dl, COL_HEIGHT
    div dl
    mov cl, ah                      ; new food position's y
    movzx ax, ch
    mov dl, ROW_LENGTH
    div dl
    mov ch, ah                      ; new food position's x
    mov word [FOOD_POS], cx
    ret


times 510-($-$$) db 0x00
dw 0xaa55
