    .data
 L1:
    .asciz "%d"

.text
    .global main
    .type main, %function
main:
      stmfd sp!,{fp,lr,v1,v2,v3,v4,v5}
      add fp,sp,#24
      sub sp,fp,#32
      mov v6, #1
      mov v7, #2
      ldr a1,=L1
      ldr a2, v6
      bl printf(PLT)

.L1exit:
 mov a4,#0
 mov a1,r3
      sub sp,fp,#24
      ldmfd sp!,{fp,pc,v1,v2,v3,v4,v5}