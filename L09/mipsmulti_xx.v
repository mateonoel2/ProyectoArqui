module mips(input  clk, reset,
            output [31:0] adr, writedata,
            output memwrite,
            input [31:0] readdata);

  wire        zero, pcen, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst;
  wire [1:0]  alusrcb, pcsrc;
  wire [2:0]  alucontrol;
  wire [5:0]  op, funct;

  controller c(clk, reset, op, funct, zero,
               pcen, memwrite, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst, 
               alusrcb, pcsrc, alucontrol);
  datapath dp(clk, reset, 
              pcen, irwrite, regwrite,
              alusrca, iord, memtoreg, regdst,
              alusrcb, pcsrc, alucontrol,
              op, funct, zero,
              adr, writedata, readdata);
endmodule

module controller(input   clk, reset,
                  input [5:0] op, funct,
                  input  zero,
                  output pcen, memwrite, irwrite, regwrite,
                  output alusrca, iord, memtoreg, regdst,
                  output [1:0] alusrcb, pcsrc,
                  output [2:0] alucontrol);

  wire [1:0] aluop;
  wire branch, pcwrite, pcenprev;

  // Main Decoder and ALU Decoder subunits.
  maindec md(clk, reset, op,
             pcwrite, memwrite, irwrite, regwrite,
             alusrca, branch, iord, memtoreg, regdst,
             alusrcb, pcsrc, aluop);
  aludec  ad(funct, aluop, alucontrol);

  and and1 (pcenprev, zero, branch);
  or or1 (pcen, pcenprev, pcwrite);


endmodule

module aludec(input  [5:0] funct,
              input  [1:0] aluop,
              output reg [2:0] alucontrol);

always @(*)
	case(aluop)
		2'b00: alucontrol <= 3'b010;
		2'b01: alucontrol <= 3'b110;
		default: case (funct)
			6'b100000: alucontrol <= 3'b010;
			6'b100010: alucontrol <= 3'b110;
			6'b100100: alucontrol <= 3'b000;
			6'b100101: alucontrol <= 3'b001;
			6'b101010: alucontrol <= 3'b111;
			default: alucontrol <= 3'bxxx;
		endcase
	endcase
endmodule

module maindec(input  clk, reset, 
               input [5:0] op, 
               output pcwrite, memwrite, irwrite, regwrite,
               output alusrca, branch, iord, memtoreg, regdst,
               output [1:0] alusrcb, pcsrc,
               output [1:0] aluop);

  parameter   FETCH   = 4'b0000; // State 0
  parameter   DECODE  = 4'b0001; // State 1
  parameter   MEMADR  = 4'b0010;	// State 2
  parameter   MEMRD   = 4'b0011;	// State 3
  parameter   MEMWB   = 4'b0100;	// State 4
  parameter   MEMWR   = 4'b0101;	// State 5
  parameter   RTYPEEX = 4'b0110;	// State 6
  parameter   RTYPEWB = 4'b0111;	// State 7
  parameter   BEQEX   = 4'b1000;	// State 8
  parameter   ADDIEX  = 4'b1001;	// State 9
  parameter   ADDIWB  = 4'b1010;	// state 10
  parameter   JEX     = 4'b1011;	// State 11

  parameter   LW      = 6'b100011;	// Opcode for lw
  parameter   SW      = 6'b101011;	// Opcode for sw
  parameter   RTYPE   = 6'b000000;	// Opcode for R-type
  parameter   BEQ     = 6'b000100;	// Opcode for beq
  parameter   ADDI    = 6'b001000;	// Opcode for addi
  parameter   J       = 6'b000010;	// Opcode for j

  reg [3:0]  state, nextstate;
  reg [14:0] controls;

  // state register
  always @(posedge clk or posedge reset)			
    if(reset) state <= FETCH;
    else state <= nextstate;

  always @ (*)
    case(state)
      FETCH:   nextstate <= DECODE;
      DECODE:  case(op)
                 LW:      nextstate <= MEMADR;
                 SW:      nextstate <= MEMADR;
                 RTYPE:   nextstate <= RTYPEEX;
                 BEQ:     nextstate <= BEQEX;
                 ADDI:    nextstate <= ADDIEX;
                 J:       nextstate <= JEX;
                 default: nextstate <= 4'bx; // should never happen
               endcase
 		// Add code here
      MEMADR: case(op)
      		 LW:	  nextstate <= MEMRD;
		       SW:	  nextstate <= MEMWR;
	 endcase
      MEMRD:   nextstate <=  MEMWB;
      MEMWB:   nextstate <= FETCH;
      MEMWR:   nextstate <= FETCH;
      RTYPEEX: nextstate <= RTYPEWB;
      RTYPEWB: nextstate <= FETCH;
      BEQEX:   nextstate <= FETCH;
      ADDIEX:  nextstate <= ADDIWB;
      ADDIWB:  nextstate <= FETCH;
      JEX:     nextstate <= FETCH;
      default: nextstate <= 4'bx; // should never happen
    endcase

  // output logic
  assign {pcwrite, memwrite, irwrite, regwrite, alusrca, branch, iord, memtoreg, regdst,alusrcb, pcsrc, aluop} = controls;

  always @ (*)
    case(state)
      FETCH:   controls <= 15'h5010;
      DECODE:  controls <= 15'h0030;
      MEMADR: controls <= 15'h0420;
      MEMRD: controls <= 15'h0100;
      MEMWB: controls <= 15'h0880;
      MEMWR: controls <= 15'h2100;
      RTYPEEX: controls <= 15'h0402;
      RTYPEWB: controls <= 15'h0840;
      BEQEX: controls <= 15'h0605;
      ADDIEX: controls <= 15'h0420;
      ADDIWB: controls <= 15'h0800;
      JEX: controls <= 15'h4008; 
    
      default: controls <= 15'hxxxx; // should never happen
    endcase
endmodule


module datapath(input  clk, reset,
                input  pcen, irwrite, regwrite,
                input  alusrca, iord, memtoreg, regdst,
                input  [1:0]  alusrcb, pcsrc, 
                input  [2:0]  alucontrol,
                output [5:0]  op, funct,
                output zero,
                output [31:0] adr, writedata, 
                input  [31:0] readdata);

  // Below are the internal signals of the datapath module.

  wire [4:0]  writeregi;
  wire [4:0] a3;
  wire [31:0] pcnext, pc;
  wire [31:0] instr, data, srca, srcb;
  wire [31:0] a;
  wire [31:0] rd;
  wire [31:0] aluresult, aluout;
  wire [31:0] signimm;   // the sign-extended immediate
  wire [31:0] signimmsh;	// the sign-extended immediate shifted left by 2
  wire [31:0] wd3, rd1, rd2;
  wire [27:0] sl2temp;
  wire [31:0] pcjump;

  // op and funct fields to controller
  assign op = instr[31:26];
  assign funct = instr[5:0];

  mux2  #(5) muxA3 (instr[20:16],instr[15:11],regdst, a3);
  mux2  #(32) muxwd3 (aluout, data, memtoreg, wd3);
  regfile regfile_1 (clk, regwrite, instr[25:21], instr[20:16], a3, wd3, rd1, rd2);
  flopr  #(32) flop_a (clk, reset, rd1, a);
  flopr  #(32) flop_b (clk, reset, rd2, writedata);
  mux2  #(32) muxsrcA (pc, a, alusrca, srca );
  

  alu alumain (srca, srcb, alucontrol, aluresult, zero);
  flopr  #(32) flopaluout (clk, reset, aluresult, aluout);
  mux3  #(32) muxpcnext (aluresult, aluout, pcjump, pcsrc, pcnext);
  flopenr  #(32) floppc (clk, reset, pcen, pcnext, pc );
  mux2  #(32) muxadr (pc, aluout, iord, adr);
  mem mem1 (clk, memwrite, adr, writedata, rd);
  flopenr #(32) flopinstr (clk, reset, irwrite, rd, instr);
  flopr  #(32) flopdata (clk, reset, rd, data);
  signext signnextsignimm (instr[15:0], signimm);
  sl2 sl2signimmsh (signimm, signimmsh);
  mux4  #(32) muxsrcb (writedata, 32'b100, signimm, signimmsh,alusrcb, srcb);
  sl225 sl2pcjump (instr[25:0], sl2temp);
  assign pcjump = {pc[31:28] , sl2temp};
endmodule

module mux3 #(parameter WIDTH = 8)
		(input [WIDTH-1:0] d0, d1, d2,
		input [1:0] s,
		output [WIDTH-1:0] y);
	assign #1 y = s[1] ? d2 : (s[0] ? d1 : d0);
endmodule

module mux4 #(parameter WITDH = 8)
		(input [WITDH-1:0] d0,d1,d2,d3,
		input [1:0] s,
		output reg [WITDH-1:0] y);

	always @(*)
		case(s)
			2'b00: y <= d0;
			2'b01: y <= d1;
			2'b10: y <= d2;
			2'b11: y <= d3;
		endcase
endmodule

