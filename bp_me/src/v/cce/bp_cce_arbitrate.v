/**
 *
 * Name:
 *   bp_cce_arbitrate.v
 *
 * Description:
 *   The arbitration unit controls access to the directory, pending bits, and
 *   speculative bits. Arbitration is between CCE modules and the microcode.
 *
 */

module bp_cce_branch
  import bp_common_pkg::*;
  import bp_common_aviary_pkg::*;
  import bp_cce_pkg::*;
  import bp_common_cfg_link_pkg::*;
  import bp_me_pkg::*;
  #(parameter bp_params_e bp_params_p = e_bp_inv_cfg
    `declare_bp_proc_params(bp_params_p)

    // Derived Parameters
    , localparam lce_assoc_width_lp = `BSG_SAFE_CLOG2(lce_max_assoc_p)
  )
  (

   // Directory Write from Microcode
   , input [paddr_width_p-1:0]                  dir_addr_i
   , input                                      dir_addr_bypass_i
   , input [lce_id_width_p-1:0]                 dir_lce_i
   , input [lce_assoc_width_lp-1:0]             dir_way_i
   , input bp_coh_states_e                      dir_coh_state_i
   , input bp_cce_inst_minor_dir_op_e           dir_cmd_i
   , input                                      dir_w_v_i

   // Directory Write from Message
   , input [paddr_width_p-1:0]                  msg_dir_addr_i
   , input                                      msg_dir_addr_bypass_i
   , input [lce_id_width_p-1:0]                 msg_dir_lce_i
   , input [lce_assoc_width_lp-1:0]             msg_dir_way_i
   , input bp_coh_states_e                      msg_dir_coh_state_i
   , input bp_cce_inst_minor_dir_op_e           msg_dir_w_cmd_i
   , input                                      msg_dir_w_v_i

   // Directory Write Output
   , output logic [paddr_width_p-1:0]           dir_addr_o
   , output logic                               dir_addr_bypass_o
   , output logic [lce_id_width_p-1:0]          dir_lce_o
   , output logic [lce_assoc_width_lp-1:0]      dir_way_o
   , output bp_coh_states_e                     dir_coh_state_o
   , output bp_cce_inst_minor_dir_op_e          dir_cmd_o
   , output logic                               dir_w_v_o

   // Pending Write from Microcode
   , input                                      pending_w_v_i
   , input [paddr_width_p-1:0]                  pending_w_addr_i
   , input                                      pending_w_addr_bypass_i
   , input                                      pending_i

   // Pending Write from Message
   // busy signal might not be needed here if ucode push/pop can cause message unit to write
   // pending bit. Just arbitrate based on the msg_pending_w_v_i
   , input                                      msg_pending_w_busy_i
   , input                                      msg_pending_w_v_i
   , input [paddr_width_p-1:0]                  msg_pending_w_addr_i
   , input                                      msg_pending_w_addr_bypass_i
   , input                                      msg_pending_i

   // Pending Write Output
   , output logic                               pending_w_v_o
   , output logic [paddr_width_p-1:0]           pending_w_addr_o
   , output logic                               pending_w_addr_bypass_o
   , output logic                               pending_o

   // Spec Write from Microcode
   , input                                      spec_w_v_i
   , input [paddr_width_p-1:0]                  spec_w_addr_i
   , input                                      spec_w_addr_bypass_i
   , input                                      spec_v_i
   , input                                      squash_v_i
   , input                                      fwd_mod_v_i
   , input                                      state_v_i
   , input bp_cce_spec_s                        spec_i

   // Spec Write from Message
   , input                                      msg_spec_w_v_i
   , input [paddr_width_p-1:0]                  msg_spec_w_addr_i
   , input                                      msg_spec_w_addr_bypass_i
   , input                                      msg_spec_v_i
   , input                                      msg_squash_v_i
   , input                                      msg_fwd_mod_v_i
   , input                                      msg_state_v_i
   , input bp_cce_spec_s                        msg_spec_i

   // Spec Write Output
   , output logic                               spec_w_v_o
   , output logic [paddr_width_p-1:0]           spec_w_addr_o
   , output logic                               spec_w_addr_bypass_o
   , output logic                               spec_v_o
   , output logic                               squash_v_o
   , output logic                               fwd_mod_v_o
   , output logic                               state_v_o
   , output bp_cce_spec_s                       spec_o

   // Spec Read from Microcode
   , input                                      spec_r_v_i
   , input [paddr_width_p-1:0]                  spec_r_addr_i
   , input                                      spec_r_addr_bypass_i

   // Spec Write from Message
   , input                                      msg_spec_r_v_i
   , input [paddr_width_p-1:0]                  msg_spec_r_addr_i
   , input                                      msg_spec_r_addr_bypass_i

   // Spec Read Output
   , output logic                               spec_r_v_o
   , output logic [paddr_width_p-1:0]           spec_r_addr_o
   , output logic                               spec_r_addr_bypass_o

  );

  always_comb begin

    // Directory Write
    if (msg_dir_w_v_i) begin
      dir_addr_o        = msg_dir_addr_i;
      dir_addr_bypass_o = msg_dir_addr_bypass_i;
      dir_lce_o         = msg_dir_lce_i;
      dir_way_o         = msg_dir_way_i;
      dir_coh_state_o   = msg_dir_coh_state_i;
      dir_cmd_o         = msg_dir_w_cmd_i;
      dir_w_v_o         = msg_dir_w_v_i;
    end else begin
      dir_addr_o        = dir_addr_i;
      dir_addr_bypass_o = dir_addr_bypass_i;
      dir_lce_o         = dir_lce_i;
      dir_way_o         = dir_way_i;
      dir_coh_state_o   = dir_coh_state_i;
      dir_cmd_o         = dir_cmd_i;
      dir_w_v_o         = dir_w_v_i;
    end

    // Pending Bit Write
    if (msg_pending_w_v_i) begin
      pending_w_v_o           = msg_pending_w_v_i;
      pending_w_addr_o        = msg_pending_w_addr_i;
      pending_w_addr_bypass_o = msg_pending_w_addr_bypass_i;
      pending_o               = msg_pending_i;
    end else begin
      pending_w_v_o           = pending_w_v_i;
      pending_w_addr_o        = pending_w_addr_i;
      pending_w_addr_bypass_o = pending_w_addr_bypass_i;
      pending_o               = pending_i;
    end

    // Spec Bits Read
    if (msg_spec_r_v_i) begin
      spec_r_v_o           = msg_spec_r_v_i;
      spec_r_addr_o        = msg_spec_r_addr_i;
      spec_r_addr_bypass_o = msg_spec_r_addr_bypass_i;
    end else begin
      spec_r_v_o           = spec_r_v_i;
      spec_r_addr_o        = spec_r_addr_i;
      spec_r_addr_bypass_o = spec_r_addr_bypass_i;
    end

    // Spec Bits Write
    if (msg_spec_w_v_i) begin
      spec_w_v_o           = msg_spec_w_v_i;
      spec_w_addr_o        = msg_spec_w_addr_i;
      spec_w_addr_bypass_o = msg_spec_w_addr_bypass_i;
      spec_v_o             = msg_spec_v_i;
      squash_v_o           = msg_squash_v_i;
      fwd_mod_v_o          = msg_fwd_mod_v_i;
      state_v_o            = msg_state_v_i;
      spec_o               = msg_spec_i;
    end else begin
      spec_w_v_o           = spec_w_v_i;
      spec_w_addr_o        = spec_w_addr_i;
      spec_w_addr_bypass_o = spec_w_addr_bypass_i;
      spec_v_o             = spec_v_i;
      squash_v_o           = squash_v_i;
      fwd_mod_v_o          = fwd_mod_v_i;
      state_v_o            = state_v_i;
      spec_o               = spec_i;
    end

  end

endmodule