                section         .text

                global          _start

_start:
                mov             r15, 0

again:
                                
                call            read_char
                cmp             word [buf], 10
                je              new_line
                jmp             again

new_line:
                add             r15, 1
                jmp             again

read_char:
                mov             rax, 0
                mov             rdi, 0
                mov             rsi, buf
                mov             rdx, 1
                syscall
                cmp             rax, 0
                jl              read_fail
                je              exit
                ret

exit:
                mov             rax, r15
                call            write_number

                mov             rax, 60
                xor             rdi, rdi
                syscall

write_number:
                mov             rbp, rsp
                mov             rdi, rsp
                sub             rsp, 24

                dec             rdi
                mov             byte [rdi], 10

                or              rax, rax
                jz              .write_zero

                mov             ebx, 10
.loop:
                xor             edx, edx
                div             rbx

                add             edx, '0'
                dec             rdi
                mov             byte [rdi], dl

                or              rax, rax
                jnz             .loop
                jmp             .print

.write_zero:
                dec             rdi
                mov             byte [rdi], '0'

.print:
                mov             eax, 1
                mov             rsi, rdi
                mov             rdx, rbp
                sub             rdx, rdi
                mov             edi, eax
                syscall

                mov             rsp, rbp
                ret

read_fail:
write_fail:
                ud2

                section         .bss
buf:            resw            1