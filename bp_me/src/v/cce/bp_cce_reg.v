/**
 *
 * Name:
 *   bp_cce_reg.v
 *
 * Description:
 *
 */

module bp_cce_reg
  import bp_common_pkg::*;
  import bp_common_aviary_pkg::*;
  import bp_cce_pkg::*;
  import bp_me_pkg::*;
  import bp_common_cfg_link_pkg::*;
  #(parameter bp_params_e bp_params_p = e_bp_inv_cfg
    `declare_bp_proc_params(bp_params_p)

    // Derived parameters
    , localparam block_size_in_bytes_lp    = (cce_block_width_p/8)
    , localparam lg_block_size_in_bytes_lp = `BSG_SAFE_CLOG2(block_size_in_bytes_lp)

    // number of bits required to represent any possible LCE associativity in the system
    , localparam lce_assoc_width_lp        = `BSG_SAFE_CLOG2(lce_max_assoc_p)

    , localparam mshr_width_lp = `bp_cce_mshr_width(lce_id_width_p, lce_max_assoc_p, paddr_width_p)

    // Interface Widths
    `declare_bp_lce_cce_if_header_widths(cce_id_width_p, lce_id_width_p, lce_max_assoc_p, paddr_width_p)
    `declare_bp_lce_cce_if_widths(cce_id_width_p, lce_id_width_p, paddr_width_p, lce_max_assoc_p, dword_width_p, cce_block_width_p)
    `declare_bp_me_if_widths(paddr_width_p, cce_block_width_p, lce_id_width_p, lce_max_assoc_p)
  )
  (input                                                                   clk_i
   , input                                                                 reset_i

   , input bp_cce_inst_decoded_s                                           decoded_inst_i

   , input [lce_cce_req_width_lp-1:0]                                      lce_req_i
   , input [lce_cce_resp_width_lp-1:0]                                     lce_resp_i
   , input [cce_mem_msg_width_lp-1:0]                                      mem_resp_i

   , input [`bp_cce_inst_gpr_width-1:0]                                    src_a_i
   , input [`bp_cce_inst_gpr_width-1:0]                                    src_b_i
   , input [`bp_cce_inst_gpr_width-1:0]                                    alu_res_i

   , input                                                                 pending_i

   , input                                                                 dir_lru_v_i
   , input                                                                 dir_lru_cached_excl_i
   , input [paddr_width_p-1:0]                                             dir_lru_addr_i

   , input                                                                 dir_addr_v_i
   , input [paddr_width_p-1:0]                                             dir_addr_i

   , input [lce_assoc_width_p-1:0]                                         gad_req_addr_way_i
   , input [lce_id_width_p-1:0]                                            gad_owner_lce_i
   , input [lce_assoc_width_lp-1:0]                                        gad_owner_lce_way_i
   , input                                                                 gad_transfer_flag_i
   , input                                                                 gad_replacement_flag_i
   , input                                                                 gad_upgrade_flag_i
   , input                                                                 gad_invalidate_flag_i
   , input                                                                 gad_cached_flag_i
   , input                                                                 gad_cached_exclusive_flag_i
   , input                                                                 gad_cached_owned_flag_i
   , input                                                                 gad_cached_dirty_flag_i

   // Register outputs
   , output logic [mshr_width_lp-1:0]                                      mshr_o
   , output logic [`bp_cce_inst_num_gpr-1:0][`bp_cce_inst_gpr_width-1:0]   gpr_o
   , output bp_coh_states_e                                                coh_state_o

  );


  // Interface Structs
  `declare_bp_me_if(paddr_width_p, cce_block_width_p, lce_id_width_p, lce_max_assoc_p);
  `declare_bp_lce_cce_if(cce_id_width_p, lce_id_width_p, paddr_width_p, lce_max_assoc_p, dword_width_p, cce_block_width_p);

  bp_lce_cce_req_s  lce_req;
  bp_lce_cce_resp_s lce_resp;
  bp_cce_mem_msg_s  mem_resp;

  assign lce_req  = lce_req_i;
  assign lce_resp = lce_resp_i;
  assign mem_resp = mem_resp_i;

  // Registers
  `declare_bp_cce_mshr_s(lce_id_width_p, lce_max_assoc_p, paddr_width_p);

  bp_cce_mshr_s                                                mshr_r, mshr_n;
  logic [`bp_cce_inst_num_gpr-1:0][`bp_cce_inst_gpr_width-1:0] gpr_r, gpr_n;
  bp_coh_states_e                                              coh_state_r, coh_state_n;

  assign mshr_o = mshr_r;
  assign gpr_o = gpr_r;
  assign coh_state_o = coh_state_r;

  // Next value for any register
  logic [`bp_cce_inst_gpr_width-1:0] gpr_next;

  // Derived control signals
  wire lce_req_rqf = (lce_req.msg_type == e_lce_req_type_wr)
                     | (lce_req.msg_type == e_lce_req_type_uc_wr);
  wire lce_req_ucf = (lce_req.msg_type == e_lce_req_type_uc_rd)
                     | (lce_req.msg_type == e_lce_req_type_uc_wr);
  wire lce_resp_nwbf = (lce_resp.msg_type == e_lce_cce_resp_null_wb);

  always_comb begin
    // By default, all registers hold their value
    mshr_n = mshr_r;
    gpr_n = gpr_r;
    coh_state_r = coh_state_n;

    // Default Coherence State Register
    coh_state_n = bp_coh_states_e'(src_a_i[0+:$bits(bp_coh_states_e)]);

    // GPRs
    // Only one GPR can be written at a time.
    // By default, use the result from the ALU
    // A Flag operation may also use the ALU and store the result into a GPR
    gpr_next = alu_res_i;
    if (decoded_inst_i.op == e_op_data) begin
      gpr_next = src_a_i;
    end else if (decoded_inst_i.op == e_op_queue) begin
      // TODO: poph sets src_a properly to lce_resp.msg_type or mem_resp.msg_type
      gpr_next = src_a_i;
    end else if (dir_addr_v_i) begin
      // TODO: this is broken because the gpr w mask from the instruction won't be valid
      // in the cycle that the directory produces the result of the RDE instruction.
      gpr_next = dir_addr_i;
    end

    // MSHR
    if (decoded_inst_i.mshr_clear) begin
      mshr_n = '0;
      mshr_n.next_coh_state = coh_state_r;
    end else begin
      // LCE ID
      // TODO: from lce_req, lce_resp, mem_resp.payload(?), or move
      mshr_n.lce_id = lce_req.header.src_id;

      // paddr
      // TODO: from lce_req, lce_resp, mem_resp, or move
      mshr_n.paddr = lce_req.header.addr;

      // Way ID
      // TODO: from GAD, mem_resp.payload(?), or move
      if (decoded_inst_i.gad_v) begin
        mshr_n.way_id = gad_req_addr_way_i;
      end else if (decoded_inst_i.way_w_v) begin
        mshr_n.way_id = src_a_i[0+:lce_id_width_p];
      end

      // LRU paddr
      // TODO: from Directory or move
      mshr_n.lru_paddr = dir_lru_addr_i;

      // LRU Way ID
      // TODO: from lce_req or move
      mshr_n.lru_way_id = lce_req.header.lru_way_id;

      // Owner LCE ID
      // TODO: from GAD or move
      mshr_n.owner_lce_id = gad_owner_lce_i;

      // Owner Way ID
      // TODO: from GAD or move
      mshr_n.owner_way_id = gad_owner_lce_way_i;

      // Next Coh State
      // TODO: from move or mem_resp.payload(?)
      mshr_n.next_coh_state = bp_coh_states_e'(decoded_inst_i.imm[0+:$bits(bp_coh_states_e)]);

      // UC Req Size
      // TODO: from lce_req or move
      mshr_n.uc_req_size = lce_req.header.uc_size;

      // Data Length
      // TODO: from lce_req, lce_resp, or move
      mshr_n.data_length = lce_req.header.data_length;

      // Flags
      case (decoded_inst_i.rqf_sel)
        e_rqf_lce_req: begin
          mshr_n.flags[e_flag_sel_rqf] = lce_req_rqf;
        end
        e_rqf_pending: begin
          mshr_n.flags[e_flag_sel_rqf] = '0; // TODO: v2
        end
        e_rqf_imm0: begin
          mshr_n.flags[e_flag_sel_rqf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_rqf] = '0;
        end
      endcase

      case (decoded_inst_i.ucf_sel)
        e_ucf_lce_req: begin
          mshr_n.flags[e_flag_sel_ucf] = lce_req_ucf;
        end
        e_ucf_pending: begin
          mshr_n.flags[e_flag_sel_ucf] = '0;
        end
        e_ucf_imm0: begin
          mshr_n.flags[e_flag_sel_ucf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_ucf] = '0;
        end
      endcase

      case (decoded_inst_i.nerf_sel)
        e_nerf_lce_req: begin
          mshr_n.flags[e_flag_sel_nerf] = lce_req.header.non_exclusive;
        end
        e_nerf_pending: begin
          mshr_n.flags[e_flag_sel_nerf] = '0; // TODO: v2
        end
        e_nerf_imm0: begin
          mshr_n.flags[e_flag_sel_nerf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_nerf] = '0;
        end
      endcase

      case (decoded_inst_i.ldf_sel)
        e_ldf_lce_req: begin
          mshr_n.flags[e_flag_sel_ldf] = lce_req.header.lru_dirty;
        end
        e_ldf_pending: begin
          mshr_n.flags[e_flag_sel_ldf] = '0; // TODO: v2
        end
        e_ldf_imm0: begin
          mshr_n.flags[e_flag_sel_ldf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_ldf] = '0;
        end
      endcase

      case (decoded_inst_i.nwbf_sel)
        e_nwbf_lce_resp: begin
          mshr_n.flags[e_flag_sel_nwbf] = lce_resp_nwbf;
        end
        e_nwbf_imm0: begin
          mshr_n.flags[e_flag_sel_nwbf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_nwbf] = '0;
        end
      endcase

      case (decoded_inst_i.tf_sel)
        e_tf_logic: begin
          mshr_n.flags[e_flag_sel_tf] = gad_transfer_flag_i;
        end
        e_tf_imm0: begin
          mshr_n.flags[e_flag_sel_tf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_tf] = '0;
        end
      endcase

      case (decoded_inst_i.pf_sel)
        e_pf_logic: begin
          mshr_n.flags[e_flag_sel_pf] = pending_o_i; // RDP instruction
        end
        e_pf_imm0: begin
          mshr_n.flags[e_flag_sel_pf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_pf] = '0;
        end
      endcase

      case (decoded_inst_i.rf_sel)
        e_rf_logic: begin
          mshr_n.flags[e_flag_sel_rf] = gad_replacement_flag_i;
        end
        e_rf_imm0: begin
          mshr_n.flags[e_flag_sel_rf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_rf] = '0;
        end
      endcase

      case (decoded_inst_i.uf_sel)
        e_uf_logic: begin
          mshr_n.flags[e_flag_sel_uf] = gad_upgrade_flag_i;
        end
        e_uf_imm0: begin
          mshr_n.flags[e_flag_sel_uf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_uf] = '0;
        end
      endcase

      case (decoded_inst_i.if_sel)
        e_if_logic: begin
          mshr_n.flags[e_flag_sel_if] = gad_invalidate_flag_i;
        end
        e_if_imm0: begin
          mshr_n.flags[e_flag_sel_if] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_if] = '0;
        end
      endcase

      case (decoded_inst_i.cf_sel)
        e_cf_logic: begin
          mshr_n.flags[e_flag_sel_cf] = gad_cached_flag_i;
        end
        e_cf_imm0: begin
          mshr_n.flags[e_flag_sel_cf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cf] = '0;
        end
      endcase

      case (decoded_inst_i.cef_sel)
        e_cef_logic: begin
          mshr_n.flags[e_flag_sel_cef] = gad_cached_exclusive_flag_i;
        end
        e_cef_imm0: begin
          mshr_n.flags[e_flag_sel_cef] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cef] = '0;
        end
      endcase

      case (decoded_inst_i.cof_sel)
        e_cof_logic: begin
          mshr_n.flags[e_flag_sel_cof] = gad_cached_owned_flag_i;
        end
        e_cof_imm0: begin
          mshr_n.flags[e_flag_sel_cof] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cof] = '0;
        end
      endcase

      case (decoded_inst_i.cdf_sel)
        e_cdf_logic: begin
          mshr_n.flags[e_flag_sel_cdf] = gad_cached_dirty_flag_i;
        end
        e_cdf_imm0: begin
          mshr_n.flags[e_flag_sel_cdf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cdf] = '0;
        end
      endcase

      case (decoded_inst_i.lef_sel)
        e_lef_logic: begin
          mshr_n.flags[e_flag_sel_lef] = dir_lru_cached_excl_i;
        end
        e_lef_imm0: begin
          mshr_n.flags[e_flag_sel_lef] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_lef] = '0;
        end
      endcase

      // TODO: SF flag might not be correct; it also might not be used
      case (decoded_inst_i.sf_sel)
        e_sf_logic: begin
          mshr_n.flags[e_flag_sel_sf] = decoded_inst_i.spec_bits.spec;
        end
        e_sf_imm0: begin
          mshr_n.flags[e_flag_sel_sf] = decoded_inst_i.imm[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_sf] = '0;
        end
      endcase

      // TODO: flags or flags_and_mask as source
      // TODO: writing flags may need to stall with something?
      //mshr_n.flags = src_a_i;

    end

  end


  // TODO: register write conditions

  always_ff @(posedge clk_i)
  begin
    if (reset_i) begin
      mshr_r <= '0;
      gpr_r <= '0;
      coh_state_r <= e_COH_I;
    end else begin
      // MSHR writes
      if (decoded_inst_i.mshr_clear) begin
        mshr_r <= mshr_n;
      end else begin
        if (decoded_inst_i.req_w_v) begin
          mshr_r.lce_id <= mshr_n.lce_id;
          mshr_r.paddr <= mshr_n.paddr;
        end
        if (decoded_inst_i.req_addr_way_w_v) begin
          mshr_r.way_id <= mshr_n.way_id;
        end
        if (decoded_inst_i.lru_way_w_v) begin
          mshr_r.lru_way_id <= mshr_n.lru_way_id;
        end
        if (decoded_inst_i.owner_lce_w_v) begin
          mshr_r.owner_lce_id <= mshr_n.owner_lce_id;
          mshr_r.owner_way_id <= mshr_n.owner_way_id;
        end
        // Flags
        for (int i = 0; i < `bp_cce_inst_num_flags; i=i+1) begin
          if (decoded_inst_i.flag_mask_w_v[i]) begin
            mshr_r.flags[i] <= mshr_n.flags[i];
          end
        end
        // TODO: ensure that this write occurs even if stalling current instruction
        // because directory is busy
        if (dir_lru_v_i) begin
          mshr_r.flags[e_flag_sel_lef] <= mshr_n.flags[e_flag_sel_lef];
          mshr_r.lru_paddr <= mshr_n.lru_paddr;
        end
        // Next Coh State
        if (decoded_inst_i.mov_dst_w_v & (decoded_inst_i.dst_sel == e_dst_sel_special)
            & (decoded_inst_i.dst.special == e_dst_next_coh_state)) begin
          mshr_r.next_coh_state <= mshr_n.next_coh_state;
        end

        if (decoded_inst_i.uc_req_size_w_v) begin
          mshr_r.uc_req_size <= mshr_n.uc_req_size;
        end
      end

      // GPR
      for (int i = 0; i < `bp_cce_inst_num_gpr; i=i+1) begin
        if (decoded_inst_i.gpr_w_mask[i]) begin
          gpr_r[i] <= gpr_n[i];
        end
      end

      if (decoded_inst_i.mov_dst_w_v & (decoded_inst_i.dst_sel == e_dst_sel_special)
          & (decoded_inst_i.dst.special == e_dst_coh_state)) begin
        coh_state_r <= coh_state_n;
      end

    end // else
  end // always_ff

endmodule
