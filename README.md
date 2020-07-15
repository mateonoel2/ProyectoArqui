# ProyectoArqui

* Para correr programa escribir los siguientes comandos:

$ iverilog -o a.out alu.v mipsmem.v mipsmulti_xx.v mipsparts.v mipstest.v topmulti.v 

$ vvp a.out

* Se generará el archivo mipstest.vcd, donde podrá ver los waveforms
