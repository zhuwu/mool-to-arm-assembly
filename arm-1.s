.data
	
L1:
	.asciz "%d\n"
	
L2:
	.asciz "%s\n"
	
L3:
	.asciz "true"
	
L4:
	.asciz "false"
	
L5:
	.asciz ""
	
L6:
	.asciz "Greater or equal than 0."
	.text
	.global main
	
main:
	stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
	add fp,sp,#24
	sub sp,fp,#24
	mov v4,#1
	mov v5,#2
	add v3,v4,#1
	mov v4,v5
	add v2,v4,#1
	add v3,v4,#1
	cmp v4,#0
	movgt v1,#1
	movle v1,#0
	cmp v1,#0
	beq .1
	ldr a1,=L1
	mov a2,v4
	bl printf(PLT)
	b .2
	
.1:
	add v4,v4,#1
	
.2:
	add v5,v3,#2
	
.3:
	cmp v5,#0
	movge v1,#1
	movlt v1,#0
	cmp v1,#0
	beq .4
	ldr a1,=L2
	ldr a2,=L6
	bl printf(PLT)
	sub v5,v5,#1
	b .3
	
.4:
	ldr a1,=L1
	mov a2,v4
	bl printf(PLT)
	ldr a1,=L1
	mov a2,v5
	bl printf(PLT)
	ldr a1,=L1
	mov a2,v3
	bl printf(PLT)
	ldr a1,=L1
	mov a2,v2
	bl printf(PLT)
	b .main_exit
	
.main_exit:
	sub sp,fp,#24
	ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}
