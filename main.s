global _start

    extern malloc
    extern free
    extern printf
    extern signal

SIGINT equ 2

struc _6502_CPUREGS
    .PC resw 1
    .F  resb 1
    .A  resb 1
    .X  resb 1
    .Y  resb 1
    .S  resb 1
endstruc

_FLAG_N equ 0x80
_FLAG_V equ 0x40
_FLAG_B equ 0x10
_FLAG_D equ 0x08
_FLAG_I equ 0x04
_FLAG_Z equ 0x02
_FLAG_C equ 0x01

%macro TRANSFLAGS_ZSC 1
    pushf
    pop ax
    mov bl, al
    and al, 0x80+%1   ; keep S flag only, which is N on 6502 and C flag is optional
    xor cl, cl
    rcl bl, 2       ; rotate Z into carry (bit 6 -> C)
    rcl cl, 2       ; rotate carry into Z (C -> bit 1)
    or al, cl       ; al contains Z and N flag
    and byte [sregs + _6502_CPUREGS.F], ~(_FLAG_Z | _FLAG_N | _FLAG_C*%1 )
    or byte [sregs + _6502_CPUREGS.F], al
%endmacro

%macro PUSH6_AL 0
    xor rbx, rbx
    mov rcx, qword [procmem]
    mov bl, byte [sregs + _6502_CPUREGS.S]
    mov byte [rcx + rbx + 0x100], al
    dec byte [sregs + _6502_CPUREGS.S]
%endmacro

%macro LOAD_STACK_TO_RSI 0
    xor rbx, rbx
    mov rsi, qword [procmem]
    mov bl, byte [sregs + _6502_CPUREGS.S]
    add rsi, rbx
    add rsi, 0x100 
%endmacro

%macro POP6_AL 0
    xor rbx, rbx
    mov rcx, qword [procmem]
    mov bl, byte [sregs + _6502_CPUREGS.S]
    mov al, byte [rcx + rbx + 0x100]
    inc byte [sregs + _6502_CPUREGS.S]
%endmacro

section .text

_6502_RAMSIZE equ 65536    

_sigint_handler:
    mov byte [done], 1
    ret

_sigint_restorer:
    mov rax, 15
    syscall
    ret

fwait:
    ; fetch the current terminal settings
    mov rax, 16    ; __NR_ioctl
    mov rdi, 0     ; fd: stdin
    mov rsi, 21505 ; cmd: TCGETS
    mov rdx, orig  ; arg: the buffer, orig
    syscall

    ; again, but this time for the 'new' buffer
    mov rax, 16
    mov rdi, 0
    mov rsi, 21505
    mov rdx, new
    syscall

    ; change settings
    and dword [new+0], -1516    ; ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON)
    and dword [new+4], -2       ; ~OPOST
    and dword [new+12], -32844  ; ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN)
    and dword [new+8], -305     ; ~(CSIZE | PARENB)
    or  dword [new+8], 48        ; CS8

    ; set settings (with ioctl again)
    mov rax, 16    ; __NR_ioctl
    mov rdi, 0     ; fd: stdin
    mov rsi, 21506 ; cmd: TCSETS
    mov rdx, new   ; arg: the buffer, new
    syscall

    ; read a character
    mov rax, 0     ; __NR_read
    mov rdi, 0     ; fd: stdin
    mov rsi, char  ; buf: the temporary buffer, char
    mov rdx, 1     ; count: the length of the buffer, 1
    syscall

    ; reset settings (with ioctl again)
    mov rax, 16    ; __NR_ioctl
    mov rdi, 0     ; fd: stdin
    mov rsi, 21506 ; cmd: TCSETS
    mov rdx, orig  ; arg: the buffer, orig
    syscall

    ret

_print:
    mov rax, 1
    mov rdi, 1
    mov rdx, 13
    syscall
    ret

_init_6502_memory:
    mov rdi, _6502_RAMSIZE
    call malloc
    test rax, rax
    jz _init_6502_memory_error
    mov [procmem], rax
    ret
_init_6502_memory_error:
    mov rsi, mem_init_err_msg
    call _print
    ret

_cleanup:
    sub rsp, 8
    mov rdi, [procmem]
    call free
    add rsp, 8
    ret

_step_cpu:
    ; LOAD BYTE FROM 6502 MEMORY
    xor rax, rax
    mov ax, word [sregs + _6502_CPUREGS.PC]
    mov rsi, [procmem]
    add rsi, rax
    xor rax, rax
    lodsb
    inc word [sregs + _6502_CPUREGS.PC]
    ror al, 2       ; rotate byte to align opcode table
    mov rbx, rax    ; save copy
    mov rcx, rax
    and al, 0xF8    ; mask adress mode
    and bl, 0x03    ; mask opcode type
    cmp al, 0x38
    jbe nosetup_src_dest
    call setup_src_dest
nosetup_src_dest:
    call [machinecode + rax]
    ret

_init_sigint_handler:
    sub rsp, 8
    mov rsi, _sigint_handler
    mov rdi, SIGINT 
    call signal
    add rsp, 8
    ret

_start:
    mov rsi, message
    call _print
    call _init_6502_memory

    ;call fwait
    call _init_sigint_handler

    mov rdi, message2
    call printf

_mainloop:
    call _step_cpu
    test byte [done], 1
    loopz _mainloop

    mov rdi, message3
    call printf

    call _cleanup
    mov rax, 60
    xor rdi, rdi
    syscall

setup_src_dest:
    xor rax, rax
    cmp bl, 1 ;          zpg
    jne lsd0
    lodsb               ; al = zeropage offset
    mov qword [inst_operand], rax
    inc word [sregs + _6502_CPUREGS.PC]
    ret
lsd0:                   
    cmp bl, 5 ;          zpg, X
    jne lsd1
    lodsb
    and cl, 0xF0
    cmp cl, 0xA0        ; for opcode 0xA0 (STX) and 0xA8 (LDX)
    je _A0_use_Y        ; use zpg,Y instead of zpg,X
    add al, byte [sregs + _6502_CPUREGS.X]
    jmp _A0_cont
_A0_use_Y:
    add al, byte [sregs + _6502_CPUREGS.Y]
_A0_cont:
    add rax, procmem
    mov qword [inst_operand], rax
    inc word [sregs + _6502_CPUREGS.PC]
    ret
lsd1:
    cmp bl, 3 ;          abs
    jne lsd2
    lodsb
    xchg al, ah
    lodsb
    xchg al, ah
    add rax, procmem
    mov qword [inst_operand], rax
    inc word [sregs + _6502_CPUREGS.PC]
    inc word [sregs + _6502_CPUREGS.PC]
    ret
lsd2:
    cmp bl, 7 ;          abs, X
    jne lsd3
    lodsb
    xchg al, ah
    lodsb
    xchg al, ah
    cmp cl, 0xA8
    jne _nomodify_7_A8
    add al, byte [sregs + _6502_CPUREGS.Y]
    jmp _cont_7_A8
_nomodify_7_A8:
    add al, byte [sregs + _6502_CPUREGS.X]
_cont_7_A8:
    adc ah, 0
    add rax, procmem
    mov qword [inst_operand], rax
    inc word [sregs + _6502_CPUREGS.PC]
    inc word [sregs + _6502_CPUREGS.PC]
    ret
lsd3:
    cmp bl, 6 ;          abs, Y
    jne lsd4
    and cl, 0xF0
    cmp cl, 0xA0        ; for opcode 0xA0 (TXS) and 0xA8 (TSX)
    je _A0_store_to_S
    lodsb
    xchg al, ah
    lodsb
    xchg al, ah
    add al, byte [sregs + _6502_CPUREGS.Y]
    adc ah, 0
    add rax, procmem
    mov qword [inst_operand], rax
    inc word [sregs + _6502_CPUREGS.PC]
    inc word [sregs + _6502_CPUREGS.PC]
    ret
_A0_store_to_S:
    mov rax, sregs + _6502_CPUREGS.S
    mov qword [inst_operand], rax 
    ret
lsd4:
    cmp bl, 0 ;          X, ind
    jne lsd5
    lodsb
    add al, byte [sregs + _6502_CPUREGS.X]
    add rax, procmem
    mov ax, word [rax]
    add rax, procmem
    mov qword [inst_operand], rax 
    inc word [sregs + _6502_CPUREGS.PC]
    ret
lsd5:
    cmp bl, 4 ;          ind, Y
    jne lsd6
    lodsb
    add rax, procmem
    mov ax, word [rax]
    add al, byte [sregs + _6502_CPUREGS.Y]
    adc ah, 0
    test byte [sregs + _6502_CPUREGS.F], _FLAG_C
    jz lsd_nocarry5
    inc word [sregs + _6502_CPUREGS.PC]
    inc ax
lsd_nocarry5:
    add rax, procmem
    mov qword [inst_operand], rax
    ret
lsd6:
    cmp bl, 2 ;
    jne lsd7
    cmp cl, 0xB0
    je _DEX
    mov rax, sregs + _6502_CPUREGS.A
    mov qword [inst_operand], rax
    ret
_DEX:
    mov rax, sregs + _6502_CPUREGS.X
    mov qword [inst_operand], rax
lsd7:
    ret


_interpret_line00:
    cmp bl, 0
    jnz l00n1
    ; BRK                               
    xor rax, rax
    mov ax, word [sregs + _6502_CPUREGS.PC]
    inc ax
    inc ax
    xchg al, ah
    PUSH6_AL        ; store PC(hi)
    xchg al, ah          
    PUSH6_AL        ; store PC(lo)
    mov al, byte [sregs + _6502_CPUREGS.F]
    or al, _FLAG_B  ; set B flag
    PUSH6_AL        ; store FLAGS
    mov rsi, qword [procmem]
    add rsi, 0xFFFE
    lodsw           ; fetch PC(lo), then fetch PC(hi)
    mov word [sregs + _6502_CPUREGS.PC], ax
    ret
l00n1:   
    cmp bl, 2
    jnz l00n2
    ; PHP                               
    mov al, byte [sregs + _6502_CPUREGS.F]
    PUSH6_AL
    ret
l00n2:
    cmp bl, 4
    jnz l00n3
    ; BPL rel                           TODO
    ret
l00n3:
    cmp bl, 6
    jnz l00n4
    ; CLC
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_C
    ret
l00n4:
    ret

_interpret_line08:
    cmp bl, 0
    jnz l08n1
    ; JSR abs                           TODO 
    ret
l08n1:   
    cmp bl, 1
    jnz l08n1b
    ; BIT zpg                           TODO
    ret
l08n1b:
    cmp bl, 2
    jnz l08n2
    ; PLP                               
    POP6_AL
    mov byte [sregs + _6502_CPUREGS.F], al
    ret
l08n2:
    cmp bl, 3
    jnz l08n2b
    ; BIT abs                           TODO
    ret
l08n2b:
    cmp bl, 4
    jnz l08n3
    ; BMI rel                           TODO
    ret
l08n3:
    cmp bl, 6
    jnz l08n4
    ; SEC
    or byte [sregs + _6502_CPUREGS.F], _FLAG_C
    ret
l08n4:

    ret

_interpret_line10:
    cmp bl, 0
    jnz l10n1
    ; RTI                               TODO 
    ret
l10n1:   
    cmp bl, 2
    jnz l10n2
    ; PHA                               
    mov al, byte [sregs + _6502_CPUREGS.A]
    PUSH6_AL
    ret
l10n2:
    cmp bl, 3
    jnz l10n2b
    ; JMP abs                           TODO
    ret
l10n2b:
    cmp bl, 4
    jnz l10n3
    ; BVC rel                           TODO
    ret
l10n3:
    cmp bl, 6
    jnz l10n4
    ; CLI
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_I
    ret
l10n4:
    ret

_interpret_line18:
    cmp bl, 0
    jnz l18n1
    ; RTS                               TODO 
    ret
l18n1:   
    cmp bl, 2
    jnz l18n2
    ; PLA
    POP6_AL
    mov byte [sregs + _6502_CPUREGS.A], al
    TRANSFLAGS_ZSC 1
    ret
l18n2:
    cmp bl, 3
    jnz l18n2b
    ; JMP ind                           TODO
    ret
l18n2b:
    cmp bl, 4
    jnz l18n3
    ; BVS rel                           TODO
    ret
l18n3:
    cmp bl, 6
    jnz l18n4
    ; SEI
    or byte [sregs + _6502_CPUREGS.F], _FLAG_I
    ret
l18n4:

    ret

_interpret_line20:
    cmp bl, 1
    jnz l20n1
    ; STY zpg                           TODO
    ret
l20n1:   
    cmp bl, 2
    jnz l20n2
    ; DEY
    dec byte [sregs + _6502_CPUREGS.Y]
    jns l20n1_nosign
    pushf
    or byte [sregs + _6502_CPUREGS.F], _FLAG_N
    popf
l20n1_nosign:
    pushf
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_N
    popf
    jnz l20n1_nozero
    or byte [sregs + _6502_CPUREGS.F], _FLAG_Z
l20n1_nozero:
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_Z
    ret
l20n2:
    cmp bl, 3
    jnz l20n2b
    ; STY abs                           TODO
    ret
l20n2b:
    cmp bl, 4
    jnz l20n3
    ; BCC rel                           TODO
    ret
l20n3:
    cmp bl, 5
    jnz l20n3b
    ; STY zpg, X                        TODO
    ret
l20n3b:
    cmp bl, 6
    jnz l20n4
    ; TYA                               TODO
    ret
l20n4:
    ret

_interpret_line28:
    cmp bl, 0
    jnz l28n0
    ; LDY #                             TODO
    ret
l28n0:
    cmp bl, 1
    jnz l28n1
    ; LDY zpg                           TODO
    ret
l28n1:
    cmp bl, 2
    jnz l28n2
    ; TAY                               TODO
    ret
l28n2:
    cmp bl, 3
    jnz l28n2b
    ; LDY abs                           TODO
    ret
l28n2b:
    cmp bl, 4
    jnz l28n3
    ; BCS rel                           TODO
    ret
l28n3:
    cmp bl, 5
    jnz l28n3b
    ; LDY zpg, x                        TODO
    ret
l28n3b:
    cmp bl, 6
    jnz l28n4
    ; CLV
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_V
    ret
l28n4:
    ; LDY abs, X                        TODO
    ret

_interpret_line30:
    cmp bl, 0
    jnz l30n1
    ; CPY #                             TODO
    ret
l30n1:   
    cmp bl, 1
    jnz l30n2
    ; CPY zpg                           TODO
    ret
l30n2:
    cmp bl, 2
    jnz l30n2b
    ; INY
    inc byte [sregs + _6502_CPUREGS.Y]
    jns l30n2_nosign
    pushf
    or byte [sregs + _6502_CPUREGS.F], _FLAG_N
    popf
l30n2_nosign:
    pushf
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_N
    popf
    jnz l30n2_nozero
    or byte [sregs + _6502_CPUREGS.F], _FLAG_Z
l30n2_nozero:
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_Z
    ret
l30n2b:
    cmp bl, 3
    jnz l30n3
    ; CPY abs                           TODO
    ret
l30n3:
    cmp bl, 4
    jnz l30n3b
    ; BNE rel                           TODO
    ret
l30n3b:
    cmp bl, 6
    jnz l30n4
    ; CLD
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_D
    ret
l30n4:
    ret

_interpret_line38:
    cmp bl, 0
    jnz l38n1
    ; CPX #                             TODO
    ret
l38n1:   
    cmp bl, 1
    jnz l38n2
    ; CPX zpg                           TODO
    ret
l38n2:
    cmp bl, 2
    jnz l38n2b
    ; INX
    inc byte [sregs + _6502_CPUREGS.X]
    jns l38n2_nosign
    pushf
    or byte [sregs + _6502_CPUREGS.F], _FLAG_N
    popf
l38n2_nosign:
    pushf
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_N
    popf
    jnz l38n2_nozero
    or byte [sregs + _6502_CPUREGS.F], _FLAG_Z
l38n2_nozero:
    and byte [sregs + _6502_CPUREGS.F], ~_FLAG_Z
    ret
l38n2b:
    cmp bl, 3
    jnz l38n3
    ; CPX abs                           TODO
    ret
l38n3:
    cmp bl, 4
    jnz l38n3b
    ; BEQ rel                           TODO
    ret
l38n3b:
    cmp bl, 6
    jnz l38n4
    ; SED
    or byte [sregs + _6502_CPUREGS.F], _FLAG_D
    ret
l38n4:
    ret

_do_ORA:
    mov rax, qword [inst_operand]
    mov al, byte [rax]
    cmp bl, 2
    jne _do_ORAb
    lodsb        ; use immediate data instead of A
_do_ORAb:
    or [sregs + _6502_CPUREGS.A], al
    TRANSFLAGS_ZSC 0
    ret

_do_AND:
    mov rax, qword [inst_operand]
    mov al, byte [rax]
    cmp bl, 2
    jne _do_ANDb
    lodsb        ; use immediate data instead of A
_do_ANDb:
    and [sregs + _6502_CPUREGS.A], al
    TRANSFLAGS_ZSC 0
    ret

_do_EOR:
    mov rax, qword [inst_operand]
    mov al, byte [rax]
    cmp bl, 2
    jne _do_EORb
    lodsb        ; use immediate data instead of A
_do_EORb:
    xor [sregs + _6502_CPUREGS.A], al
    TRANSFLAGS_ZSC 0
    ret

_do_ADC:
                                         ;TODO
    ret

_do_STA:
    mov cl, byte [sregs + _6502_CPUREGS.A]
    mov rax, qword [inst_operand]
    mov byte [rax], cl
    ret

_do_LDA:
    mov rax, qword [inst_operand]
    mov al, byte [rax]
    cmp bl, 2
    jne _do_LDAb
    lodsb        ; use immediate data instead of A
_do_LDAb:
    mov byte [sregs + _6502_CPUREGS.A], al
    TRANSFLAGS_ZSC 0
    ret

_do_CMP:
    mov rax, qword [inst_operand]
    mov al, byte [rax]
    cmp bl, 2
    jne _do_CMPb
    lodsb       ; use immediate data instead of A
_do_CMPb:
    mov bl, byte [sregs + _6502_CPUREGS.A]
    sub bl, al
    TRANSFLAGS_ZSC 1
    ret

_do_SBC:
                                        ; TODO
    ret

_do_ASL:
    mov rax, qword [inst_operand]
    clc
    rcl byte [rax], 1
    TRANSFLAGS_ZSC 1
    ret

_do_ROL:
    mov rax, qword [inst_operand]
    test byte [sregs + _6502_CPUREGS.S], 1
    jz _do_ROL_nc
    stc
_do_ROL_nc: 
    rcl byte [rax], 1
    TRANSFLAGS_ZSC 1
    ret

_do_LSR:
    mov rax, qword [inst_operand]
    clc
    rcr byte [rax], 1
    TRANSFLAGS_ZSC 1
    ret

_do_ROR:
    mov rax, qword [inst_operand]
    test byte [sregs + _6502_CPUREGS.S], 1
    jz _do_ROR_nc
    stc
_do_ROR_nc: 
    rcr byte [rax], 1
    TRANSFLAGS_ZSC 1
    ret

_do_STX:
    mov rax, qword [inst_operand]
    mov cl, byte [sregs + _6502_CPUREGS.X]
    mov byte [rax], cl
    ret

_do_LDX:
    mov rax, qword [inst_operand]
    mov cl, byte [rax]
    mov byte [sregs + _6502_CPUREGS.X], cl
    TRANSFLAGS_ZSC 0
    ret

_do_DEC:
    mov rax, qword [inst_operand]
    dec byte [inst_operand]
    TRANSFLAGS_ZSC 0
    ret

_do_INC:
    cmp bl, 2
    je _NOP
    mov rax, qword [inst_operand]
    inc byte [inst_operand]
    TRANSFLAGS_ZSC 0
    ret
_NOP:
    nop ; :)) haha
    ret

section .rodata
message db "Hello, World", 0x0A, 0x00
mem_init_err_msg db "Error initializing 6502 memory", 0x0A, 0x00
message2 db "Sigint Handler has been setup", 0x0A, 0x00
message3 db "Exiting Program.", 0x0A, 0x00
errorsighandler db "Error setting up signal handler", 0x0A, 0x00

machinecode dq  _interpret_line00
            dq  _interpret_line08
            dq  _interpret_line10
            dq  _interpret_line18
            dq  _interpret_line20
            dq  _interpret_line28
            dq  _interpret_line30
            dq  _interpret_line38
            dq  _do_ORA
            dq  _do_AND
            dq  _do_EOR
            dq  _do_ADC
            dq  _do_STA
            dq  _do_LDA
            dq  _do_CMP
            dq  _do_SBC
            dq  _do_ASL
            dq  _do_ROL
            dq  _do_LSR
            dq  _do_ROR
            dq  _do_STX
            dq  _do_LDX
            dq  _do_DEC
            dq  _do_INC
                

section .data
    procmem dq 0

orig times 80 db 0
new  times 80 db 0
char times  2 db 0

sregs:
    istruc _6502_CPUREGS
        at _6502_CPUREGS.PC, dw 0
        at _6502_CPUREGS.F, db 255 
        at _6502_CPUREGS.A, db 0
        at _6502_CPUREGS.X, db 0
        at _6502_CPUREGS.Y, db 0
        at _6502_CPUREGS.S, db 0
    iend

inst_operand dq sregs + _6502_CPUREGS.A
immdata db 0
    
done db 0
