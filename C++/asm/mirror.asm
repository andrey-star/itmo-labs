; no read or write fails	
				section 		.text
read:			
				mov				rax, 0
				mov				rdi, 0
				mov				rsi, buf
				mov				rdx, 1024
				syscall

				cmp 			rax, 0
				je				exit
				jmp				write

write:
				mov				rax, 1
				mov				rdi, 1
				mov				rsi, buf
				mov				rdx, 1024
				syscall

				jmp 			read

exit:
				mov             rax, 60
                mov             rdi, 0
                syscall


				section			.data
buf:			resb			1024