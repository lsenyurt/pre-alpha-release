/**
 *
 * Name:
 *   bp_cce_branch.v
 *
 * Description:
 *   Branch evaluation logic for the CCE implementing equality/inequality and less than comparison.
 *   The branch unit also performs misprediction resolution, if needed.
 *
 *   Some microcode operations such as bgt are implemented via assembler/softwware transforms.
 *   See bp_cce_inst.vh for SW supported operations.
 *
 *   The operand width is parameterizable, and set based on the microarchitecture design.
 *
 *   The branch unit also computes and outputs the correct next PC for all instructions.
 *
 */

module bp_cce_branch
  import bp_cce_pkg::*;
  #(parameter width_p          = "inv"
    , parameter cce_pc_width_p = "inv"
  )
  (input [width_p-1:0]                       opd_a_i
   , input [width_p-1:0]                     opd_b_i
   , input                                   branch_i
   , input                                   predicted_taken_i
   , input bp_cce_inst_branch_op_e           branch_op_i
   , input [cce_pc_width_p-1:0]              execute_pc_i
   , input [cce_pc_width_p-1:0]              branch_target_i
   , output logic                            branch_res_o
   , output logic                            mispredict_o
   , output logic [cce_pc_width_p-1:0]       pc_o
  );

  // TODO: branch_res_o needed?
  // it is useful within the module (although easily substituted)

  wire equal = (opd_a_i == opd_b_i);
  wire not_equal = ~equal;
  wire less = (opd_a_i < opd_b_i);

  wire [cce_pce_width_p-1:0] execute_pc_plus_one;

  logic branch_res;
  always_comb begin : branch
    unique case (branch_op_i)
      e_branch_eq_op:  branch_res = equal;
      e_branch_neq_op: branch_res = not_equal;
      e_branch_lt_op:  branch_res = less;
      e_branch_le_op:  branch_res = (less | equal);
      default:         branch_res = '0;
    endcase

    // gate branch result output on branch valid input signal
    branch_res_o = branch_i & branch_res;

    // Misprediction happens if:
    // a) branch was predicted not taken, but branch should have been taken
    // b) branch was predicted taken, but branch should not have been taken
    mispredict_o = ((branch_res_o & !predicted_taken_i)
                    | (!branch_res_o & predicted_taken_i));

    // Output correct next PC (for all instructions)
    // If the current instruction is a branch and the branch evaluates to taken, the next PC
    // is the branch target. Else, the next PC is the current PC plus one.
    pc_o = (branch_i & branch_res)
           ? branch_target_i
           : execute_pc_plus_one;
  end

endmodule
