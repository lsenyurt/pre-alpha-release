/**
 *
 * Name:
 *   bp_cce_inst.vh
 *
 * Description:
 *   This file describes the CCE microcode instructions. Any changes made to this file must be
 *   reflected in the source code of the CCE microcode assembler, too.
 *
 *   This file defines both the assembler generated and internally decoded formats of the microcode.
 *
 *   Some software operations are supported via assembler transforms rather than being supported
 *   directly in hardware (e.g., ALU increment and decrement).
 *
 *   Note: this file may rely on defines from bsg_defines.h in the BaseJump STL repo.
 *   Note: this file relies on the LCE-CCE IF defines in bp_common_me_if.vh
 */

`ifndef BP_CCE_INST_VH
`define BP_CCE_INST_VH

/*
 * Instruction width definitions
 */

// Instructions are 32-bits wide with 2 bits of attached metadata
// cce_instr_width_p should be equal to 34, and used when passing instruction+metadata
`define bp_cce_inst_width 32
`define bp_cce_inst_metadata_width 2
`define bp_cce_inst_op_width 3
`define bp_cce_inst_minor_op_width 4

// Microcode RAM address width
// 9 bits allows up to 512 instructions
// this must be greater or equal to cce_pc_width_p in bp_common_aviary_pkg
`define bp_cce_inst_addr_width 9

// Immediate field widths
`define bp_cce_inst_imm1_width 1
`define bp_cce_inst_imm2_width 2
`define bp_cce_inst_imm4_width 4
`define bp_cce_inst_imm8_width 8
`define bp_cce_inst_imm16_width 16

/*
 * General Purpose Registers
 *
 * Note: number of GPRs must be less than or equal to the number that can be
 * represented in the GPR operand enum. Currently, the maximum is 16 GPRs, but only
 * 8 are actually implemented and used.
 */

`define bp_cce_inst_num_gpr 8
`define bp_cce_inst_gpr_sel_width `BSG_SAFE_CLOG2(`bp_cce_inst_num_gpr)
`define bp_cce_inst_gpr_width 64

/*
 * Major Op Codes
 */

typedef enum logic [2:0] {
  e_op_alu                               = 3'b000    // ALU operation
  ,e_op_branch                           = 3'b001    // Branch (control flow) operation
  ,e_op_data                             = 3'b010    // Register data movement operation
//,e_op_mem                              = 3'b011    // Memory data operation (not implemented)
  ,e_op_flag                             = 3'b100
  ,e_op_dir                              = 3'b101
  ,e_op_queue                            = 3'b110
//,e_op_unused                           = 3'b111
} bp_cce_inst_op_e;

/*
 * Minor Op Codes
 */

// Minor ALU Op Codes
typedef enum logic [3:0] {
  e_add_op                               = 4'b0000   // Add
//,e_nop_op                              = 4'b0000   // Null Operation (r0 = r0 + r0)
  ,e_sub_op                              = 4'b0001   // Subtract
  ,e_lsh_op                              = 4'b0010   // Left Shift
  ,e_rsh_op                              = 4'b0011   // Right Shift
  ,e_and_op                              = 4'b0100   // Bit-wise AND
  ,e_or_op                               = 4'b0101   // Bit-wise OR
  ,e_xor_op                              = 4'b0110   // Bit-wise XOR
  ,e_neg_op                              = 4'b0111   // Bit-wise negation (unary)
  ,e_addi_op                             = 4'b1000   // Add immediate
//,e_inc_op                              = 4'b1000   // Increment register by 1
  ,e_subi_op                             = 4'b1001   // Subtract immediate
//,e_dec_op                              = 4'b1001   // Decrement register by 1
  ,e_lshi_op                             = 4'b1010   // Left Shift immediate
  ,e_rshi_op                             = 4'b1011   // Right Shift immediate
} bp_cce_inst_minor_alu_op_e;

// Minor Branch Op Codes
typedef enum logic [3:0] {
  e_beq_op                               = 4'b0000   // Branch if A == B
//,e_bi_op                               = 4'b0000   // Unconditional Branch, or Branch if A == A
  ,e_bne_op                              = 4'b0001   // Branch if A != B
  ,e_blt_op                              = 4'b0010   // Branch if A < B
//,e_bgt_op                              = 4'b0010   // Branch if A > B, or B < A
  ,e_ble_op                              = 4'b0011   // Branch if A <= B
//,e_bge_op                              = 4'b0011   // Branch if A >= B, or B <= A

  ,e_bs_op                               = 4'b0100   // Branch if special == GPR
  ,e_bss_op                              = 4'b0101   // Branch if special == special

  ,e_beqi_op                             = 4'b1000   // Branch if A == immediate
//,e_bz_op                               = 4'b1000   // Branch if A == 0
  ,e_bneqi_op                            = 4'b1001   // Branch if A != immediate
//,e_bnz_op                              = 4'b1001   // Branch if A != 0

  ,e_bsi_op                              = 4'b1100   // Branch if special == immediate
} bp_cce_inst_minor_branch_op_e;

// Minor Register Data Movement Op Codes
typedef enum logic [3:0] {
  e_mov_op                               = 4'b0000   // Move GPR to GPR
  ,e_movsg_op                            = 4'b0001   // Move Special Register to GPR
  ,e_movgs_op                            = 4'b0010   // Move GPR to Special Register
  ,e_movfg_op                            = 4'b0011   // Move Flag to GPR[0]
  ,e_movgf_op                            = 4'b0100   // Move GPR[0] to Flag
  ,e_movpg_op                            = 4'b0101   // Move Param to GPR
  ,e_movi_op                             = 4'b1000   // Move Immediate to GPR
  ,e_movis_op                            = 4'b1001   // Move Immediate to Special Register
  ,e_clm_op                              = 4'b1111   // Clear MSHR register
} bp_cce_inst_minor_reg_data_op_e;

// Minor Memory Op Codes
// Note: these are not implemented in the CCE by default. In software, the e_m* operations
// operate on global memory (i.e., physical/main memory in the system). There is a bit
// in the instruction encoding to indicate local (i.e., CCE scratchpad) or global memory
// operation.
typedef enum logic [3:0] {
  e_ldb_op                               = 4'b0000   // Load byte from memory
  ,e_ldh_op                              = 4'b0001   // Load half-word from memory
  ,e_ldw_op                              = 4'b0010   // Load word from memory
  ,e_ldd_op                              = 4'b0011   // Load double-word from memory
  ,e_stb_op                              = 4'b0100   // Store byte to memory
  ,e_sth_op                              = 4'b0101   // Store half-word to memory
  ,e_stw_op                              = 4'b0110   // Store word to memory
  ,e_std_op                              = 4'b0111   // Store double-word to memory
//,e_mldb_op                             = 4'b1000   // Load byte from global memory
//,e_mldh_op                             = 4'b1001   // Load half-word from global memory
//,e_mldw_op                             = 4'b1010   // Load word from global memory
//,e_mldd_op                             = 4'b1011   // Load double-word from global memory
//,e_mstb_op                             = 4'b1100   // Store byte to global memory
//,e_msth_op                             = 4'b1101   // Store half-word to global memory
//,e_mstw_op                             = 4'b1110   // Store word to global memory
//,e_mstd_op                             = 4'b1111   // Store double-word to global memory
} bp_cce_inst_minor_mem_op_e;

// Minor Flag Op Codes
typedef enum logic [3:0] {
  e_sf_op                                = 4'b0000   // Move imm[0] = 1 to flag
//,e_sfz_op                              = 4'b0000   // Move imm[0] = 0 to flag
  ,e_andf_op                             = 4'b0001   // Logical AND two flags to GPR
  ,e_orf_op                              = 4'b0010   // Logical OR two flags to GPR
  ,e_nandf_op                            = 4'b0011   // Logical AND two flags to GPR
  ,e_norf_op                             = 4'b0100   // Logical OR two flags to GPR
  ,e_negf_op                             = 4'b0101   // Bitwise negation of flag
  ,e_ld_flags_op                         = 4'b0110   // MSHR.flags = GPR[0+:num_flags]

  ,e_bf_op                               = 4'b1000   // Branch if (MSHR.Flags & mask) == mask
  ,e_bfz_op                              = 4'b1001   // Branch if (MSHR.Flags & mask) == 0
  ,e_bfnz_op                             = 4'b1010   // Branch if (MSHR.Flags & mask) != 0
  ,e_bfnot_op                            = 4'b1011   // Branch if (MSHR.Flags & mask) != mask
  ,e_ld_flags_i_op                       = 4'b1100   // MSHR.flags = imm[0+:num_flags]
//,e_clf_op                              = 4'b1100   // MSHR.flags = 0
} bp_cce_inst_minor_flag_op_e;

// Minor Directory Op Codes
typedef enum logic [3:0] {
  e_rdp_op                               = 4'b0000   // Read Pending Bit
  ,e_rdw_op                              = 4'b0001   // Read Directory Way Group
  ,e_rde_op                              = 4'b0010   // Read Directory Entry
  ,e_wdp_op                              = 4'b0100   // Write Pending Bit
  ,e_clp_op                              = 4'b0101   // Clear Pending Bit
  ,e_clr_op                              = 4'b0110   // Clear Directory Row
  ,e_wde_op                              = 4'b0110   // Write Directory Entry
  ,e_wds_op                              = 4'b0111   // Write Directory Entry State
  ,e_gad_op                              = 4'b1000   // Generate Auxiliary Data
} bp_cce_inst_minor_dir_op_e;

// Minor Queue Op Codes
// 1. poph does not dequeue data or memory, but captures the standard header fields into the MSHR,
//    and also captures the message type into the specified GPR.
// 2. popd dequeues a single 64-bit data packet into a single GPR. The user must first have at
//    at least done a poph to determine that data was available and so ucode can use data_length
//    field in MSHR to determine how many packets to dequeue.
// 3. popq dequeues only the header. We assume that all data has been popped off
//    either by popd commands, or by the message unit auto-forward mechanism, or by issuing
//    a pushq command that consumes the data (e.g., an explicit pushq memCmd that consumes an
//    lceResp containing writeback data).

typedef enum logic [3:0] {
  e_wfq_op                               = 4'b0000   // Wait for Queue Valid
  ,e_pushq_op                            = 4'b0001   // Push Queue
//,e_pushqc_op                           = 4'b0001   // Push Queue Custom Message
  ,e_popq_op                             = 4'b0010   // Pop Queue - dequeue the header
  ,e_poph_op                             = 4'b0011   // Pop Header From Queue - does not pop message
  ,e_popd_op                             = 4'b0100   // Pop Data From Queue
  ,e_wspecq_op                           = 4'b0101   // Write speculative access bits
  ,e_rspecq_op                           = 4'b0110   // Read speculative access bits to GPR
  ,e_inv_op                              = 4'b1000   // Send all Invalidations based on sharers vector
} bp_cce_inst_minor_queue_op_e;

// Minor Op Code Union
typedef union packed {
  bp_cce_inst_minor_alu_op_e             alu_minor_op;
  bp_cce_inst_minor_branch_op_e          branch_minor_op;
  bp_cce_inst_minor_reg_data_op_e        reg_data_minor_op;
//bp_cce_inst_minor_mem_op_e             mem_minor_op;
  bp_cce_inst_minor_flag_op_e            flag_minor_op;
  bp_cce_inst_minor_dir_op_e             dir_minor_op;
  bp_cce_inst_minor_queue_op_e           queue_minor_op;
  //                                     unused op
} bp_cce_inst_minor_op_u;


/*
 * ALU Unit Operation
 */
typedef enum logic [2:0] {
  e_add_op                               = 3'b000   // Add
  ,e_sub_op                              = 3'b001   // Subtract
  ,e_lsh_op                              = 3'b010   // Left Shift
  ,e_rsh_op                              = 3'b011   // Right Shift
  ,e_and_op                              = 3'b100   // Bit-wise AND
  ,e_or_op                               = 3'b101   // Bit-wise OR
  ,e_xor_op                              = 3'b110   // Bit-wise XOR
  ,e_neg_op                              = 3'b111   // Bit-wise negation (unary)
} bp_cce_inst_alu_op_e;

`define bp_cce_inst_alu_op_width $bits(bp_cce_inst_alu_op_e)

/*
 * Branch Unit Operation
 */
typedef enum logic [1:0] {
  e_branch_eq                            = 2'b00   // Branch if A == B
  ,e_branch_neq                          = 2'b01   // Branch if A != B
  ,e_branch_lt                           = 2'b10   // Branch if A < B
  ,e_branch_le                           = 2'b11   // Branch if A <= B
} bp_cce_inst_branch_op_e;

`define bp_cce_inst_branch_op_width $bits(bp_cce_inst_branch_op_e)

/*
 * Speculative Bits Unit Operation
 */
typedef enum logic [3:0] {
  e_spec_set                             = 4'b0000 // Set spec bit to 1
  ,e_spec_unset                          = 4'b0001 // Set spec bit to 0
  ,e_spec_squash                         = 4'b0010 // Set squash bit to 1, clear spec bit
  ,e_spec_fwd_mod                        = 4'b0011 // Set fwd_mod bit to 1, clear spec bit, set state to state
  ,e_spec_clear                          = 4'b0111 // Write all fields of speculative access bits to 0
  ,e_spec_rd_spec                        = 4'b1000 // Read spec bit to GPR
} bp_cce_inst_spec_op_e;

`define bp_cce_inst_spec_op_width $bits(bp_cce_inst_spec_op_e)

// Struct that defines speculative memory access tracking metadata
// This is used in the decoded instruction and the bp_cce_spec module
typedef struct packed
{
  logic                          spec;
  logic                          squash;
  logic                          fwd_mod;
  bp_coh_states_e                state;
} bp_cce_spec_s;


/*
 * Operand Selects
 */

`define bp_cce_inst_opd_width 4

// GPR Operand Select
// GPR's can be source or destination
typedef enum logic [3:0] {
  e_opd_r0                               = 4'b0000
  ,e_opd_r1                              = 4'b0001
  ,e_opd_r2                              = 4'b0010
  ,e_opd_r3                              = 4'b0011
  ,e_opd_r4                              = 4'b0100
  ,e_opd_r5                              = 4'b0101
  ,e_opd_r6                              = 4'b0110
  ,e_opd_r7                              = 4'b0111
} bp_cce_inst_opd_gpr_e;

// Flag Operand Select
// Flags can be source or destination
typedef enum logic [3:0] {
  e_opd_rqf                              = 4'b0000
  ,e_opd_ucf                             = 4'b0001
  ,e_opd_nerf                            = 4'b0010
  ,e_opd_ldf                             = 4'b0011
  ,e_opd_pf                              = 4'b0100
  ,e_opd_lef                             = 4'b0101
  ,e_opd_cf                              = 4'b0110
  ,e_opd_cef                             = 4'b0111
  ,e_opd_cof                             = 4'b1000
  ,e_opd_cdf                             = 4'b1001
  ,e_opd_tf                              = 4'b1010
  ,e_opd_rf                              = 4'b1011
  ,e_opd_uf                              = 4'b1100
  // note: invalidate conditions should be more flexible
  ,e_opd_if                              = 4'b1101
  ,e_opd_nwbf                            = 4'b1110
  ,e_opd_sf                              = 4'b1111

// TODO: rethink flags for generality
// e_cce_flag_rc  -- requestor has block cached in any valid state
// e_cce_flag_rcs -- requestor cached shared (1) exclusive/not-shared (0)
// e_cce_flag_rco -- requestor cached owned (1)
// e_cce_flag_rcd -- requestor cached possibly dirty (1) guaranteed clean (0)

} bp_cce_inst_opd_flag_e;

// Control Flag one hot encoding
typedef enum logic [15:0] {
  e_cce_flag_rqf_mask                    = 16'b0000_0000_0000_0001 // request type flag
  ,e_cce_flag_ucf_mask                   = 16'b0000_0000_0000_0010 // uncached request flag
  ,e_cce_flag_nerf_mask                  = 16'b0000_0000_0000_0100 // non-exclusive request flag
  ,e_cce_flag_ldf_mask                   = 16'b0000_0000_0000_1000 // lru dirty flag
  ,e_cce_flag_pf_mask                    = 16'b0000_0000_0001_0000 // pending flag
  ,e_cce_flag_lef_mask                   = 16'b0000_0000_0010_0000 // lru cached exclusive flag
  ,e_cce_flag_cf_mask                    = 16'b0000_0000_0100_0000 // cached by other flag
  ,e_cce_flag_cef_mask                   = 16'b0000_0000_1000_0000 // cached exclusive by other flag
  ,e_cce_flag_cof_mask                   = 16'b0000_0001_0000_0000 // cached owned by other flag
  ,e_cce_flag_cdf_mask                   = 16'b0000_0010_0000_0000 // cached dirty by other flag
  ,e_cce_flag_tf_mask                    = 16'b0000_0100_0000_0000 // transfer flag
  ,e_cce_flag_rf_mask                    = 16'b0000_1000_0000_0000 // replacement flag
  ,e_cce_flag_uf_mask                    = 16'b0001_0000_0000_0000 // upgrade flag
  ,e_cce_flag_if_mask                    = 16'b0010_0000_0000_0000 // invalidate flag
  ,e_cce_flag_nwbf_mask                  = 16'b0100_0000_0000_0000 // null writeback flag
  ,e_cce_flag_sf_mask                    = 16'b1000_0000_0000_0000 // speculative flag
} bp_cce_inst_flag_onehot_e;

`define bp_cce_inst_num_flags $bits(bp_cce_inst_flag_onehot_e)

// Special Operand Select
typedef enum logic [3:0] {
  // MSHR fields can be source or destination
  e_opd_req_lce                          = 4'b0000 // MSHR.lce_id
  ,e_opd_req_addr                        = 4'b0001 // MSHR.paddr
  ,e_opd_req_way                         = 4'b0010 // MSHR.way_id
  ,e_opd_lru_addr                        = 4'b0011 // MSHR.lru_paddr
  ,e_opd_lru_way                         = 4'b0100 // MSHR.lru_way_id
  ,e_opd_owner_lce                       = 4'b0101 // MSHR.owner_lce_id
  ,e_opd_owner_way                       = 4'b0110 // MSHR.owner_way_id
  ,e_opd_next_coh_state                  = 4'b0111 // MSHR.next_coh_state
  ,e_opd_flags                           = 4'b1000 // MSHR.flags
  ,e_opd_uc_req_size                     = 4'b1001 // MSHR.uc_req_size
  ,e_opd_data_length                     = 4'b1010 // MSHR.data_length

  // sharers vectors require src_a/b to provide GPR rX containing index to use
  // These can only be sources, not destinations
  ,e_opd_sharers_hit                     = 4'b1101 // sharers_hits[rX]
  ,e_opd_sharers_way                     = 4'b1110 // sharers_ways[rX]
  ,e_opd_sharers_state                   = 4'b1111 // sharers_states[rX]
} bp_cce_inst_opd_special_e;

`define bp_cce_inst_opd_special_width $bits(bp_cce_inst_opd_special_e)

// Params Operand Select
typedef enum logic [3:0] {
  // These four parameters can only be sources
  e_opd_cce_id                           = 4'b0000 // ID of this CCE
  ,e_opd_num_lce                         = 4'b0001 // total number of LCE in system
  ,e_opd_num_cce                         = 4'b0010 // total number of CCE in system
  ,e_opd_num_wg                          = 4'b0011 // Number of WG managed by this CCE
  // The following can be source or destination
  ,e_opd_auto_fwd_msg                    = 4'b0100 // Message auto-forward control
  ,e_opd_coh_state_default               = 4'b0101 // Default for MSHR.next_coh_state
} bp_cce_inst_opd_params_e;

`define bp_cce_inst_opd_params_width $bits(bp_cce_inst_opd_params_e)

// Queue valid signals and message types
// These can only be used as sources
typedef enum logic [3:0] {
  e_opd_mem_resp_v                       = 4'b0000
  ,e_opd_lce_resp_v                      = 4'b0001
  ,e_opd_pending_v                       = 4'b0010
  ,e_opd_lce_req_v                       = 4'b0011
  ,e_opd_lce_resp_type                   = 4'b0100
  ,e_opd_mem_resp_type                   = 4'b0101
} bp_cce_inst_opd_queue_e;

`define bp_cce_inst_opd_queue_width $bits(bp_cce_inst_opd_queue_e)

/*
 * Source Operands
 */

// Source Union
typedef union packed {
  bp_cce_inst_opd_gpr_e        gpr;
  bp_cce_inst_opd_flag_e       flag;
  bp_cce_inst_opd_special_e    special;
  bp_cce_inst_opd_params_e     param;
  bp_cce_inst_opd_queue_e      queue;
} bp_cce_inst_src_u;

typedef enum logic [2:0] {
  e_src_sel_gpr
  ,e_src_sel_flag
  ,e_src_sel_special
  ,e_src_sel_param
  ,e_src_sel_queue
  ,e_src_sel_imm
} bp_cce_inst_src_sel_e;

`define bp_cce_inst_src_sel_width $bits(bp_cce_inst_src_sel_e)

/*
 * Destination Operands
 */

// Destination Union
typedef union packed {
  bp_cce_inst_opd_gpr_e        gpr;
  bp_cce_inst_opd_flag_e       flag;
  bp_cce_inst_opd_special_e    special;
  bp_cce_inst_opd_params_e     param;
} bp_cce_inst_dst_u;

typedef enum logic [1:0] {
  e_dst_sel_gpr
  ,e_dst_sel_flag
  ,e_dst_sel_special
  ,e_dst_sel_param
} bp_cce_inst_dst_sel_e;

`define bp_cce_inst_dst_sel_width $bits(bp_cce_inst_dst_sel_e)


/*
 * MUX Controls
 *
 * These are used to pick where an address, LCE ID, or way ID are sourced from for
 * various instructions, including message and directory operations.
 */

// Address
typedef enum logic [3:0] {
  e_mux_sel_addr_r0                      = 4'b0000
  ,e_mux_sel_addr_r1                     = 4'b0001
  ,e_mux_sel_addr_r2                     = 4'b0010
  ,e_mux_sel_addr_r3                     = 4'b0011
  ,e_mux_sel_addr_r4                     = 4'b0100
  ,e_mux_sel_addr_r5                     = 4'b0101
  ,e_mux_sel_addr_r6                     = 4'b0110
  ,e_mux_sel_addr_r7                     = 4'b0111
  ,e_mux_sel_addr_mshr_req               = 4'b1000
  ,e_mux_sel_addr_mshr_lru               = 4'b1001
  ,e_mux_sel_addr_lce_req                = 4'b1010
  ,e_mux_sel_addr_lce_resp               = 4'b1011
  ,e_mux_sel_addr_mem_resp               = 4'b1100
  ,e_mux_sel_addr_pending                = 4'b1101
  ,e_mux_sel_addr_0                      = 4'b1111 // constant 0
} bp_cce_inst_mux_sel_addr_e;

`define bp_cce_inst_mux_sel_addr_width $bits(bp_cce_inst_mux_sel_addr_e)

// LCE ID
typedef enum logic [3:0] {
  e_mux_sel_lce_r0                       = 4'b0000
  ,e_mux_sel_lce_r1                      = 4'b0001
  ,e_mux_sel_lce_r2                      = 4'b0010
  ,e_mux_sel_lce_r3                      = 4'b0011
  ,e_mux_sel_lce_r4                      = 4'b0100
  ,e_mux_sel_lce_r5                      = 4'b0101
  ,e_mux_sel_lce_r6                      = 4'b0110
  ,e_mux_sel_lce_r7                      = 4'b0111
  ,e_mux_sel_lce_mshr_req                = 4'b1000
  ,e_mux_sel_lce_mshr_owner              = 4'b1001
  ,e_mux_sel_lce_lce_req                 = 4'b1010
  ,e_mux_sel_lce_lce_resp                = 4'b1011
  ,e_mux_sel_lce_mem_resp                = 4'b1100
  ,e_mux_sel_lce_pending                 = 4'b1101
  ,e_mux_sel_lce_0                       = 4'b1111 // constant 0
} bp_cce_inst_mux_sel_lce_e;

`define bp_cce_inst_mux_sel_lce_width $bits(bp_cce_inst_mux_sel_lce_e)

// Way
typedef enum logic [3:0] {
  e_mux_sel_way_r0                       = 4'b0000
  ,e_mux_sel_way_r1                      = 4'b0001
  ,e_mux_sel_way_r2                      = 4'b0010
  ,e_mux_sel_way_r3                      = 4'b0011
  ,e_mux_sel_way_r4                      = 4'b0100
  ,e_mux_sel_way_r5                      = 4'b0101
  ,e_mux_sel_way_r6                      = 4'b0110
  ,e_mux_sel_way_r7                      = 4'b0111
  ,e_mux_sel_way_mshr_req                = 4'b1000
  ,e_mux_sel_way_mshr_owner              = 4'b1001
  ,e_mux_sel_way_mshr_lru                = 4'b1010
  ,e_mux_sel_way_sh_way                  = 4'b1011 // Sharer's vector ways, indexed by src_a
  ,e_mux_sel_way_0                       = 4'b1111 // constant 0
} bp_cce_inst_mux_sel_way_e;

`define bp_cce_inst_mux_sel_way_width $bits(bp_cce_inst_mux_sel_way_e)

// Coherence State
// source select for directory coherence state input
typedef enum logic [3:0] {
  e_mux_sel_coh_r0                       = 4'b0000
  ,e_mux_sel_coh_r1                      = 4'b0001
  ,e_mux_sel_coh_r2                      = 4'b0010
  ,e_mux_sel_coh_r3                      = 4'b0011
  ,e_mux_sel_coh_r4                      = 4'b0100
  ,e_mux_sel_coh_r5                      = 4'b0101
  ,e_mux_sel_coh_r6                      = 4'b0110
  ,e_mux_sel_coh_r7                      = 4'b0111
  ,e_mux_sel_coh_next_coh_state          = 4'b1000
  //,e_mux_sel_sharer_state                = 4'b1001 // Sharer's vector states, indexed by src_a
  ,e_mux_sel_coh_inst_imm                = 4'b1111
} bp_cce_inst_mux_sel_coh_state_e;

`define bp_cce_inst_mux_sel_coh_state_width $bits(bp_cce_inst_mux_sel_coh_state_e)

/*
 * Source and Destination Queue Selects and One-hot masks
 */

// Source queue one hot
// order: {lceReq, lceResp, memResp, pending}
typedef enum logic [3:0] {
  e_src_q_pending                        = 4'b0001
  ,e_src_q_mem_resp                      = 4'b0010
  ,e_src_q_lce_resp                      = 4'b0100
  ,e_src_q_lce_req                       = 4'b1000
} bp_cce_inst_src_q_e;

`define bp_cce_num_src_q $bits(bp_cce_inst_src_q_e)

// Source queue select
typedef enum logic [1:0] {
  e_src_q_sel_lce_req                    = 2'b00
  ,e_src_q_sel_mem_resp                  = 2'b01
  ,e_src_q_sel_pending                   = 2'b10
  ,e_src_q_sel_lce_resp                  = 2'b11
} bp_cce_inst_src_q_sel_e;

`define bp_cce_inst_src_q_sel_width $bits(bp_cce_inst_src_q_sel_e)

// Destination queue one hot
typedef enum logic [1:0] {
  e_dst_q_lce_cmd                        = 2'b01
  ,e_dst_q_mem_cmd                       = 2'b10
} bp_cce_inst_dst_q_e;

`define bp_cce_num_dst_q $bits(bp_cce_inst_dst_q_e)

// Destination queue select
typedef enum logic [1:0] {
  e_dst_q_lce_cmd                        = 2'b00
  ,e_dst_q_mem_cmd                       = 2'b01
} bp_cce_inst_dst_q_sel_e;

`define bp_cce_inst_dst_q_sel_width $bits(bp_cce_inst_dst_q_sel_e)


/*
 * Flag Source Selects
 *
 * All flags can be written by the microcode. Some can also be written as side-effects
 * from other instructions, e.g., GAD or popq/h.
 */

// RQF
typedef enum logic [1:0] {
  e_rqf_lce_req                          = 2'b00
  ,e_rqf_pending                         = 2'b01
  ,e_rqf_imm0                            = 2'b10
} bp_cce_inst_rqf_sel_e;

`define bp_cce_inst_rqf_sel_width $bits(bp_cce_inst_rqf_sel_e)

// UCF
typedef enum logic [1:0] {
  e_ucf_lce_req                          = 2'b00
  ,e_ucf_pending                         = 2'b01
  ,e_ucf_imm0                            = 2'b10
} bp_cce_inst_ucf_sel_e;

`define bp_cce_inst_ucf_sel_width $bits(bp_cce_inst_ucf_sel_e)

// NERF
typedef enum logic [1:0] {
  e_nerf_lce_req                         = 2'b00
  ,e_nerf_pending                        = 2'b01
  ,e_nerf_imm0                           = 2'b10
} bp_cce_inst_nerf_sel_e;

`define bp_cce_inst_nerf_sel_width $bits(bp_cce_inst_nerf_sel_e)

// LDF
typedef enum logic [1:0] {
  e_ldf_lce_req                          = 2'b00
  ,e_ldf_pending                         = 2'b01
  ,e_ldf_imm0                            = 2'b10
} bp_cce_inst_ldf_sel_e;

`define bp_cce_inst_ldf_sel_width $bits(bp_cce_inst_ldf_sel_e)

// PF
typedef enum logic {
  e_pf_logic                             = 1'b0
  ,e_pf_imm0                             = 1'b1
} bp_cce_inst_pf_sel_e;

`define bp_cce_inst_pf_sel_width $bits(bp_cce_inst_pf_sel_e)

// LEF
typedef enum logic {
  e_lef_logic                            = 1'b0
  ,e_lef_imm0                            = 1'b1
} bp_cce_inst_lef_sel_e;

`define bp_cce_inst_lef_sel_width $bits(bp_cce_inst_lef_sel_e)

// CF
typedef enum logic {
  e_cf_logic                             = 1'b0
  ,e_cf_imm0                             = 1'b1
} bp_cce_inst_cf_sel_e;

`define bp_cce_inst_cf_sel_width $bits(bp_cce_inst_cf_sel_e)

// CEF
typedef enum logic {
  e_cef_logic                            = 1'b0
  ,e_cef_imm0                            = 1'b1
} bp_cce_inst_cef_sel_e;

`define bp_cce_inst_cef_sel_width $bits(bp_cce_inst_cef_sel_e)

// COF
typedef enum logic {
  e_cof_logic                            = 1'b0
  ,e_cof_imm0                            = 1'b1
} bp_cce_inst_cof_sel_e;

`define bp_cce_inst_cof_sel_width $bits(bp_cce_inst_cof_sel_e)

// CDF
typedef enum logic {
  e_cdf_logic                            = 1'b0
  ,e_cdf_imm0                            = 1'b1
} bp_cce_inst_cdf_sel_e;

`define bp_cce_inst_cdf_sel_width $bits(bp_cce_inst_cdf_sel_e)

// TF
typedef enum logic {
  e_tf_logic                             = 1'b0
  ,e_tf_imm0                             = 1'b1
} bp_cce_inst_tf_sel_e;

`define bp_cce_inst_tf_sel_width $bits(bp_cce_inst_tf_sel_e)

// RF
typedef enum logic {
  e_rf_logic                             = 1'b0
  ,e_rf_imm0                             = 1'b1
} bp_cce_inst_rf_sel_e;

`define bp_cce_inst_rf_sel_width $bits(bp_cce_inst_rf_sel_e)

// UF
typedef enum logic {
  e_uf_logic                             = 1'b0
  ,e_uf_imm0                             = 1'b1
} bp_cce_inst_uf_sel_e;

`define bp_cce_inst_uf_sel_width $bits(bp_cce_inst_uf_sel_e)

// IF
typedef enum logic {
  e_if_logic                             = 1'b0
  ,e_if_imm0                             = 1'b1
} bp_cce_inst_if_sel_e;

`define bp_cce_inst_if_sel_width $bits(bp_cce_inst_if_sel_e)

// NWBF
typedef enum logic {
  e_nwbf_lce_resp                        = 1'b0
  ,e_nwbf_imm0                           = 1'b1
} bp_cce_inst_nwbf_sel_e;

`define bp_cce_inst_nwbf_sel_width $bits(bp_cce_inst_nwbf_sel_e)

// SF
typedef enum logic {
  e_sf_logic                             = 1'b0
  ,e_sf_imm0                             = 1'b1
} bp_cce_inst_sf_sel_e;

`define bp_cce_inst_sf_sel_width $bits(bp_cce_inst_sf_sel_e)

/*
 * Instruction Struct Definitions
 *
 * Each instruction is 32-bits wide. There are also two metadata bits attached to each
 * instruction that indicate if the instruction is a branch and if the branch should
 * be predicted taken or not. The metadata bits enable the pre-decoder to quickly decide
 * what PC should be (speculatively) fetched next.
 *
 * Each instruction contains:
 *   op (3-bits)
 *   minor_op (4-bits)
 *   instruction type specific struct with padding (25-bits)
 *
 * Any changes made to this file must be reflected in the C version used by the assembler, and
 * in the assembler itself.
 *
 */

`define bp_cce_inst_type_u_width \
  (`bp_cce_inst_width-`bp_cce_inst_op_width-`bp_cce_inst_minor_op_width)

/*
 * 2-Register Encoding
 *
 */

`define bp_cce_inst_rtype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_opd_width \
  -(2*`bp_cce_inst_opd_width))

typedef struct packed {
  logic [`bp_cce_inst_rtype_pad-1:0]     pad;
  bp_cce_inst_src_u                      src_b;
  bp_cce_inst_dst_u                      dst;
  bp_cce_inst_src_u                      src_a;
} bp_cce_inst_rtype_s;

/*
 * Immediate Encoding
 *
 */

`define bp_cce_inst_itype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_opd_width \
  -`bp_cce_inst_opd_width-`bp_cce_inst_imm16_width)

typedef struct packed {
  logic [`bp_cce_inst_imm16_width-1:0]   imm;
  logic [`bp_cce_inst_itype_pad-1:0]     pad;
  bp_cce_inst_dst_u                      dst;
  bp_cce_inst_src_u                      src_a;
} bp_cce_inst_itype_s;

/*
 * Memory Load Encoding (same as I-Type)
 * rd = mem[ra+imm]
 *
 * Src and dst can only be GPR
 */

// no padding needed

typedef struct packed {
  logic [`bp_cce_inst_imm16_width-1:0]   imm;
  logic                                  global_mem;
  bp_cce_inst_opd_gpr_e                  dst;
  bp_cce_inst_opd_gpr_e                  src_a;
} bp_cce_inst_mltype_s;

/*
 * Memory Store Encoding (basically I-Type, but second source instead of destination)
 * mem[ra+imm] = rb
 *
 * Src and dst can only be GPR
 */

// no padding needed

typedef struct packed {
  logic [`bp_cce_inst_imm16_width-1:0]   imm;
  logic                                  global_mem;
  bp_cce_inst_opd_gpr_e                  src_b;
  bp_cce_inst_opd_gpr_e                  src_a;
} bp_cce_inst_mstype_s;

/*
 * Branch Encoding
 *
 */

`define bp_cce_inst_btype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_imm4_width \
  -(2*`bp_cce_inst_opd_width)-`bp_cce_inst_addr_width)

typedef struct packed {
  logic [`bp_cce_inst_addr_width-1:0]    target;
  logic [`bp_cce_inst_btype_pad-1:0]     pad;
  bp_cce_inst_src_u                      src_b;
  logic [`bp_cce_inst_imm4_width-1:0]    pad4;
  bp_cce_inst_src_u                      src_a;
} bp_cce_inst_btype_s;

/*
 * Branch-Immediate Encoding
 *
 */

`define bp_cce_inst_bitype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_opd_width \
  -`bp_cce_inst_imm8_width-`bp_cce_inst_addr_width)

typedef struct packed {
  logic [`bp_cce_inst_addr_width-1:0]    target;
  logic [`bp_cce_inst_bitype_pad-1:0]    pad;
  logic [`bp_cce_inst_imm8_width-1:0]    imm;
  bp_cce_inst_src_u                      src_a;
} bp_cce_inst_bitype_s;

/*
 * Branch-Flag Encoding
 *
 */

// no padding, target and immediate occupy exactly 25 bits

typedef struct packed {
  logic [`bp_cce_inst_addr_width-1:0]    target;
  logic [`bp_cce_inst_imm16_width-1:0]   imm;
} bp_cce_inst_bftype_s;

/*
 * SpecQ Encoding (S-Type)
 *
 */

`define bp_cce_inst_stype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_spec_op_width \
  -$bits(bp_coh_states_e)-`bp_cce_inst_opd_width)

typedef struct packed {
  logic [`bp_cce_inst_specq_pad-1:0]     pad;
  bp_coh_states_e                        state;
  bp_cce_inst_opd_gpr_e                  dst;
  bp_cce_inst_spec_op_e                  cmd;
} bp_cce_inst_stype_s;

/*
 * Directory Pending Encoding (DP-Type)
 *
 */

`define bp_cce_inst_dptype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_mux_sel_addr_width \
  -`bp_cce_inst_opd_width-1)

typedef struct packed {
  logic [`bp_cce_inst_dptype_pad-1:0]    pad;
  logic                                  pending;
  bp_cce_inst_opd_gpr_e                  dst;
  bp_cce_inst_mux_sel_addr_e             addr_sel;
} bp_cce_inst_dptype_s;

/*
 * Directory Read Encoding (DR-Type)
 *
 */

`define bp_cce_inst_drtype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_mux_sel_addr_width \
  -`bp_cce_inst_mux_sel_lce_width-(2*`bp_cce_inst_mux_sel_way_width) \
  -`bp_cce_inst_opd_width)

typedef struct packed {
  logic [`bp_cce_inst_drtype_pad-1:0]    pad;
  bp_cce_inst_mux_sel_way_e              lru_way_sel;
  bp_cce_inst_mux_sel_way_e              way_sel;
  bp_cce_inst_mux_sel_lce_e              lce_sel;
  bp_cce_inst_opd_gpr_e                  dst;
  bp_cce_inst_mux_sel_addr_e             addr_sel;
} bp_cce_inst_drtype_s;

/*
 * Directory Write Encoding (DW-Type)
 *
 */

`define bp_cce_inst_dwtype_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_mux_sel_addr_width \
  -`bp_cce_inst_mux_sel_lce_width-`bp_cce_inst_mux_sel_way_width \
  -`bp_cce_inst_mux_sel_coh_state_width-$bits(bp_coh_states_e))

typedef struct packed {
  logic [`bp_cce_inst_dwtype_pad-1:0]    pad;
  bp_coh_states_e                        state;
  bp_cce_inst_mux_sel_way_e              way_sel;
  bp_cce_inst_mux_sel_lce_e              lce_sel;
  bp_cce_inst_mux_sel_state_e            state_sel;
  bp_cce_inst_mux_sel_addr_e             addr_sel;
} bp_cce_inst_dwtype_s;

/*
 * Pop Queue Encoding
 *
 */

`define bp_cce_inst_popq_pad (`bp_cce_inst_type_u_width-`bp_cce_inst_src_q_sel_width \
  -`bp_cce_inst_opd_width-1)

typedef struct packed {
  logic [`bp_cce_inst_popq_pad-1:0]      pad;
  bp_cce_inst_opd_gpr_e                  dst;
  logic [`bp_cce_inst_imm2_width-1:0]    pad2;
  bp_cce_inst_src_q_sel_e                src_q;
} bp_cce_inst_popq_s;

/*
 * Push Queue Encoding
 *
 */

typedef struct packed {
  logic                                  pad;
  bp_cce_inst_mux_sel_way_e              way_sel;
  bp_cce_inst_opd_gpr_e                  src_a;
  bp_cce_inst_mux_sel_lce_e              lce_sel;
  bp_cce_inst_mux_sel_addr_e             addr_sel;
  union packed
  {
    bp_lce_cmd_type_e      lce_cmd;
    bp_cce_mem_cmd_type_e  mem_cmd;
  }                                      cmd;
  logic                                  spec;
  logic                                  custom; // == 0
  bp_cce_inst_dst_q_sel_e                dst_q;
} bp_cce_inst_pushq_s;

typedef struct packed {
  logic                                  pad;
  bp_lce_cce_data_length_e               data_length;
  bp_cce_inst_opd_gpr_e                  src_a;
  bp_cce_inst_mux_sel_lce_e              lce_sel;
  bp_cce_inst_mux_sel_addr_e             addr_sel;
  union packed
  {
    bp_lce_cmd_type_e      lce_cmd;
    bp_cce_mem_cmd_type_e  mem_cmd;
  }                                      cmd;
  logic                                  spec; // unused
  logic                                  custom; // == 1
  bp_cce_inst_dst_q_sel_e                dst_q;
} bp_cce_inst_pushq_custom_s;


/*
 * Instruction Type Struct Union
 */

typedef union packed {
  bp_cce_inst_rtype_s                    rtype;
  bp_cce_inst_itype_s                    itype;
  bp_cce_inst_mltype_s                   mltype;
  bp_cce_inst_mstype_s                   mstype;
  bp_cce_inst_btype_s                    btype;
  bp_cce_inst_bitype_s                   bitype;
  bp_cce_inst_bftype_s                   bftype;
  bp_cce_inst_stype_s                    stype;
  bp_cce_inst_dptype_s                   dptype;
  bp_cce_inst_dwtype_s                   dwtype;
  bp_cce_inst_drtype_s                   drtype;
  bp_cce_inst_popq_s                     popq;
  bp_cce_inst_pushq_s                    pushq;
  bp_cce_inst_pushq_custom_s             pushq_custom;
} bp_cce_inst_type_u;

typedef struct packed {
  logic                                  predict_taken;
  logic                                  branch;
  bp_cce_inst_type_u                     type_u;
  bp_cce_inst_minor_op_u                 minor_op_u;
  bp_cce_inst_op_e                       op;
} bp_cce_inst_s;

`define bp_cce_inst_s_width $bits(bp_cce_inst_s)

/*
 * bp_cce_inst_decoded_s defines the decoded form of the CCE microcode instructions
 *
 */
typedef struct packed {

  // instruction is valid
  logic                                    v;

  // branch and predict taken bits from raw instruction
  logic                                    branch;
  logic                                    predict_taken;

  // Basic operation information
  bp_cce_inst_op_e                         op;
  bp_cce_inst_minor_op_u                   minor_op_u;

  // Destination and Source signals with selects
  bp_cce_inst_dst_u                        dst;
  bp_cce_inst_dst_sel_e                    dst_sel;
  bp_cce_inst_src_u                        src_a;
  bp_cce_inst_src_sel_e                    src_a_sel;
  bp_cce_inst_src_u                        src_b;
  bp_cce_inst_src_sel_e                    src_b_sel;

  // Address, LCE, Way, and Coherence State Selects
  // These are used by directory, pending bits, speculative bits, messages, etc.
  // note: addr_bypass signal generated by src_sel depending on mux signal
  // bypass will occur for GPR as source
  bp_cce_inst_mux_sel_addr_e               addr_sel;
  bp_cce_inst_mux_sel_lce_e                lce_sel;
  bp_cce_inst_mux_sel_way_e                way_sel;
  bp_cce_inst_mux_sel_way_e                lru_way_sel;
  bp_cce_inst_mux_sel_coh_state_e          coh_state_sel;

  // Immediate
  logic [`bp_cce_inst_gpr_width-1:0]       imm;

  // ALU Unit
  bp_cce_inst_alu_op_e                     alu_op;

  // Branch Unit
  bp_cce_inst_branch_op_e                  branch_op;
  logic [`bp_cce_inst_addr_width-1:0]      branch_target;

  // Directory
  logic                                    dir_r_v;
  logic                                    dir_w_v;
  bp_cce_inst_minor_dir_op_e               dir_op;

  // GAD Module
  logic                                    gad_v;

  // Pending Bits
  logic                                    pending_r_v;
  logic                                    pending_w_v;
  logic                                    pending_bit;
  logic                                    pending_clear;

  // Speculative Memory Access Bits
  logic                                    spec_r_v;
  logic                                    spec_w_v;
  logic                                    spec_v;
  logic                                    spec_squash_v;
  logic                                    spec_fwd_mod_v;
  logic                                    spec_state_v;
  bp_cce_spec_s                            spec_bits;

  // Message Unit
  logic                                    lce_req_yumi;
  logic                                    lce_resp_yumi;
  logic                                    mem_resp_yumi;
  logic                                    pending_yumi;
  logic                                    lce_cmd_v;
  bp_lce_cmd_type_e                        lce_cmd;
  logic                                    mem_cmd_v;
  bp_cce_mem_cmd_type_e                    mem_cmd;

  // Instruction writes the dst field specified in the decoded instruction
  logic                                    dst_w_v;

  // Flag write mask - for instructions that implicitly write flags, e.g.,
  // GAD, popq
  logic [`bp_cce_inst_num_flags-1:0]       flag_w_v;

  // Flag source selects
  bp_cce_inst_rqf_sel_e                    rqf_sel;
  bp_cce_inst_ucf_sel_e                    ucf_sel;
  bp_cce_inst_nerf_sel_e                   nerf_sel;
  bp_cce_inst_ldf_sel_e                    ldf_sel;
  bp_cce_inst_pf_sel_e                     pf_sel;
  bp_cce_inst_lef_sel_e                    lef_sel;
  bp_cce_inst_cf_sel_e                     cf_sel;
  bp_cce_inst_cef_sel_e                    cef_sel;
  bp_cce_inst_cof_sel_e                    cof_sel;
  bp_cce_inst_cdf_sel_e                    cdf_sel;
  bp_cce_inst_tf_sel_e                     tf_sel;
  bp_cce_inst_rf_sel_e                     rf_sel;
  bp_cce_inst_uf_sel_e                     uf_sel;
  bp_cce_inst_if_sel_e                     if_sel;
  bp_cce_inst_nwbf_sel_e                   nwbf_sel;
  bp_cce_inst_sf_sel_e                     sf_sel;

  // MSHR control
  // TODO: these might not all be needed. For example next_coh_state is only written
  // by an explicit movis instruction, so its write enable can be inferred from the
  // destination field and select, and use dst_w_v
  logic                                    mshr_clear;
  logic                                    lce_w_v;
  logic                                    addr_w_v;
  logic                                    way_w_v;
  logic                                    lru_addr_w_v;
  logic                                    lru_way_w_v;
  logic                                    owner_lce_w_v;
  logic                                    owner_way_w_v;
  logic                                    next_coh_state_w_v;
  logic                                    uc_req_size_w_v;
  logic                                    data_length_w_v;

} bp_cce_inst_decoded_s;

`define bp_cce_inst_decoded_width $bits(bp_cce_inst_decoded_s)

`endif
