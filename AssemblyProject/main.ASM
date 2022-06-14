

jmp start
	number 	db 5 dup 0			; stores output string
	cr     	dw 10, 13, "$"			; carriage return, line feed
	temp   	dw 0
	counter dw 0				; stores the counter of lines


start:
	mov cx,0	 			; cx will hold the current integer
	mov bl,0      				; bl will be used as counter


morechar:
	mov ah,01h				; reads a character to al
	int 21h					
	mov dx,0				; dx becomes 0 
	mov dl,al				; store the input character in dl
	mov ax,cx
	cmp dl,0D				; check if the character is enter (array is over)
	je setupdon1
	cmp dl, ' '
	je finnumjmp1		
	jne dolasgel				; if it is not enter, checks what is it

dolasgel:
	sub dx,'&'	
	cmp dx, 0		; &
	je andop1
	cmp dx, 4		;*
	je multop1
	cmp dx, 5		;+
	je addop
	cmp dx, 9 		; /
	je divop1
	add dx, '&'		;int
	sub dx, ':'	
	cmp dx, 0
	jl intop
	add dx, ':'		;A-F
	sub dx, 'G'
	cmp dx, 0
	jl hexa
	add dx, 'G'		; ^
	sub dx, '^'
	cmp dx, 0
	je xorop1
	add dx, '^'		; |
	sub dx, '|'
	cmp dx, 0
	je orop1


don:						; it enters there only the situations it is hexadecimal
	mov temp,dx				; 3 lines later, when we multiply ax and cx, dx changes. so, copy dx's value to temp
	mov ax,cx		
	mov cx,16d
	mul cx					; multiply ax by 16 (shift the number read so far) 320 + 5  
	add ax,temp				; add numeric value of new input character to the number read so far
	mov cx,ax				; put result to cx again

anadonus:
	jmp morechar


intop:
	add dx, ':'
	sub dx, '0'
	jmp don

hexa:						; it enters here only it is bigger than 10 convert it to A,B,C,D,E,F
	add dx, 'G'				; i.e  if it is 11 if we do nothing then, it will be ; from its ASCII value
	sub dx, '7'				; but when we sub '7' then it will be B instead of ;
	jmp don

setupdon1:					; for hindering jmp >128 error
	jmp setupdon2	

multop1:
	jmp multop

finnumjmp1:
	jmp finnumjmp2

addop:
	pop bx
	pop cx
	add cx, bx
	push cx
	mov cx, 0
	jmp anadonus	


andop1:
	jmp andop
divop1:
	jmp divop
xorop1:
	jmp xorop
orop1:
	jmp orop

multop:
	pop ax
	pop bx
	mul bx
	push ax
	mov cx, 0
	jmp anadonus				

andop:
	pop bx
	pop cx
	and cx, bx
	push cx
	mov cx, 0
	jmp anadonus

setupdon2:
	jmp setup_string

finnumjmp2:				
	jmp finnum



divop:
	mov dx, 0
	pop bx
	pop ax
	div bx
	push ax
	mov cx, 0
	jmp anadonus

xorop:
	pop bx
	pop cx
	xor cx, bx
	push cx
	mov cx, 0
	jmp anadonus

orop:
	pop bx
	pop cx
	or cx, bx
	push cx
	mov cx, 0
	jmp anadonus



finnum:
	cmp cx, 0
	je anadonus
	
	push cx
	mov cx, 0
	jmp anadonus

enternum:
	push cx
	jmp afternum

;=====================
; convert number to string
;=====================


setup_string:
	;result is already in ax
	mov bx,offset number+4 		; put a $ at end of buffer
	mov b[bx],"$"			; we will fill buffer from back
	dec bx

cmp cx, 0
jne enternum

afternum:
pop ax

;bx 0 dan küçükse bitsin 

convert_decimal:
	mov dx, counter
	inc dx
	mov counter, dx
	mov dx,0
	mov cx,16d
	div cx					; divide ax (i.e. current number) by 16 to get the last hexadigit	
	add dx,48d  				; convert remainder (last digit) to its ASCII representation
	cmp dx, '9'
	ja prthexa
gec:
	mov [bx],dl				; and move to buffer for output
	dec bx
	cmp counter,4				; check if we have got all digits
	jnz convert_decimal
	


print_cr:
	mov ah,09
	mov dx,offset cr			; print out a carriage return character
	int 21h


printout:
	mov dx,bx				; give the address of string to dx
	inc dx					; we decremented once too many, go forward one
	mov ah,09
	int 21h


exit:
	mov ah,04ch				; exit the program with 0
	mov al,00
	int 21h

prthexa:			
	add dx, 7d
	jmp gec

