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

   // Control signals
   , input bp_cce_inst_decoded_s                                           decoded_inst_i
   , input                                                                 dir_lru_v_i
   , input                                                                 dir_addr_v_i

   , input                                                                 stall_i

   // Data source inputs
   , input [`bp_cce_inst_gpr_width-1:0]                                    src_a_i
   , input [`bp_cce_inst_gpr_width-1:0]                                    alu_res_i

   , input [lce_cce_req_width_lp-1:0]                                      lce_req_i
   , input [lce_cce_resp_width_lp-1:0]                                     lce_resp_i
   , input [cce_mem_msg_width_lp-1:0]                                      mem_resp_i

   // For RDP, output state of pending bits from read operation
   , input                                                                 pending_i

   // From Directory - RDW operation generates LRU Cached Exclusive flag and LRU entry address
   // TODO: these can be written while the directory is busy (and ucode is thus stalling)
   , input                                                                 dir_lru_cached_excl_i
   , input [paddr_width_p-1:0]                                             dir_lru_addr_i
   // From Directory - RDE operation writes address to GPR
   // TODO: this will be written while directory is busy (and ucode is thus stalling)
   , input [paddr_width_p-1:0]                                             dir_addr_i
   , input bp_cce_inst_opd_gpr_e                                           dir_addr_dst_gpr

   // From GAD unit - written on GAD ucode operation
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
   , output logic                                                          auto_fwd_msg_o

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
  logic [`bp_cce_inst_num_gpr-1:0][`bp_cce_inst_gpr_width-1:0] gpr_r;
  logic [`bp_cce_inst_gpr_width-1:0]                           gpr_next;
  bp_coh_states_e                                              coh_state_r, coh_state_n;
  logic                                                        auto_fwd_msg_r, auto_fwd_msg_n;

  assign mshr_o = mshr_r;
  assign gpr_o = gpr_r;
  assign coh_state_o = coh_state_r;
  assign auto_fwd_msg_o = auto_fwd_msg_r;

  // Write mask for GPRs
  // This is by default the write mask from the decoded instruction, but it is also modified
  // by the Directory on RDE operation to indicate which GPR the address from RDE is written to.
  // On a stall, the mask is set to '0 by default.
  logic [`bp_cce_inst_num_gpr-1:0]                             gpr_w_mask;


  // TODO: register write conditions
  // TODO: only commit instruction if not stalling (in general)
  // However, there may be conditions where a register needs to be written during a stall

  // TODO: what state can be written during a microcode instruction stall?
  // What actions can happen during ucode stall?
  // - only action taken that is not directed by ucode is message unit auto-forwarding
  //   mem_resp to lce_cmd, auto-dequeue lce_resp (coh_ack)

  always_ff @(posedge clk_i)
  begin
    if (reset_i) begin
      mshr_r <= '0;
      gpr_r <= '0;
      coh_state_r <= e_COH_I;
      auto_fwd_msg_r <= 1'b1;
    end else begin

      // Auto Forward Message control - only from move, only when not stalling
      if (~stall_i & decoded_inst_i.auto_fwd_msg_w_v) begin
        auto_fwd_msg_r <= auto_fwd_msg_n;
      end

      // Default Coherence State for MSHR - only from move, only when not stalling
      if (~stall_i & decoded_inst_i.coh_state_w_v) begin
        coh_state_r <= coh_state_n;
      end

      // GPR
      for (int i = 0; i < `bp_cce_inst_num_gpr; i=i+1) begin
        if (gpr_w_mask[i]) begin
          gpr_r[i] <= gpr_next;
        end
      end




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

    end // else
  end // always_ff




  // Next value for any register

  // Derived control signals

  // Move operation
  wire mov_op = (decoded_inst_i.op == e_op_data);
  // Queue operation
  wire queue_op = (decoded_inst_i.op == e_op_queue);

  // Flag next values
  wire lce_req_rqf = (lce_req.msg_type == e_lce_req_type_wr)
                     | (lce_req.msg_type == e_lce_req_type_uc_wr);
  wire lce_req_ucf = (lce_req.msg_type == e_lce_req_type_uc_rd)
                     | (lce_req.msg_type == e_lce_req_type_uc_wr);
  wire lce_resp_nwbf = (lce_resp.msg_type == e_lce_cce_resp_null_wb);
  wire lce_req_nerf = (lce_req.header.non_exclusive == e_lce_req_non_excl);
  wire lce_req_ldf = (lce_req.header.lru_dirty == e_lce_req_lru_dirty);

  wire write_all_flags = ((decoded_inst_i.dst_sel == e_dst_sel_special)
                          & ((decoded_inst_i.dst.special == e_opd_flags)
                             | (decoded_inst_i.dst.special == e_opd_flags_and_mask)));


  always_comb begin
    // By default, all registers hold their value
    mshr_n = mshr_r;
    gpr_next = '0;
    coh_state_r = coh_state_n;
    auto_fwd_msg_r = auto_fwd_msg_n;

    // TODO: revise to account for message unit interactions and stalls
    // Note: choosing the next state is unaffected by the ucode stalling

    // Auto Forward BP Coherence Messages
    auto_fwd_msg_n = src_a_i[0];

    // Default Coherence State Register
    coh_state_n = bp_coh_states_e'(src_a_i[0+:$bits(bp_coh_states_e)]);

    // GPR Write Mask
    gpr_w_mask = stall_i ? '0 : decoded_inst_i.gpr_w_mask;
    // RDE operation sets write mask bit for proper GPR destination
    // this write only happens when ucode is stalling
    if (dir_addr_v_i) begin
      gpr_w_mask[dir_addr_dst_gpr[0+:`bp_cce_inst_gpr_sel_width]] = 1'b1;
    end

    // GPRs
    // By default, use the result from the ALU (ALU ops, Flag ALU ops)
    gpr_next = alu_res_i;
    // Move ops and set flag ops
    if (mov_op | queue_op) begin
      gpr_next = src_a_i;
    end else if (dir_addr_v_i) begin
      gpr_next = {'0, dir_addr_i};
    end

    // MSHR
    if (decoded_inst_i.mshr_clear) begin
      mshr_n = '0;
      mshr_n.next_coh_state = coh_state_r;
    end else begin

      // LCE ID - from lce_req, lce_resp, mem_resp.payload, or move
      // paddr - from lce_req, lce_resp, mem_resp, or move
      // LRU Way ID - from lce_req or move
      // Next Coh State - from move or mem_resp.payload
      // UC Req Size - from lce_req or move
      // Data Length - from lce_req, lce_resp, or move
      // Way ID - from move, GAD, or mem_resp
      // Owner LCE ID - from GAD or move
      // Owner Way ID - from GAD or move
      // LRU paddr - from Directory or move
      mshr_n.lce_id = src_a_i[0+:lce_id_width_p];
      mshr_n.paddr = src_a_i[0+:paddr_width_p];
      mshr_n.lru_way_id = src_a_i[0+:lce_assoc_width_lp]
      mshr_n.next_coh_state = bp_coh_states_e'(src_a_i[0+:$bits(bp_coh_states_e)]);
      mshr_n.uc_req_size = bp_lce_cce_uc_req_size_e'(src_a_i[0+:$bits(bp_lce_cce_uc_req_size_e)]);
      mshr_n.data_length = bp_lce_cce_data_length_e'(src_a_i[0+:$bits(bp_lce_cce_data_length_e)]);
      mshr_n.way_id = src_a_i[0+:lce_id_width_p];
      mshr_n.owner_lce_id = src_a_i[0+:lce_id_width_p];
      mshr_n.owner_way_id = src_a_i[0+:lce_assoc_width_lp];
      mshr_n.lru_paddr = src_a_i[0+:paddr_width_p];

      // Overrides from defaults - poph
      if (decoded_inst_i.poph) begin
        if (decoded_inst_i.popq_sel == e_src_q_sel_lce_req) begin
          mshr_n.lce_id = lce_req.header.src_id;
          mshr_n.paddr = lce_req.header.addr;
          mshr_n.lru_way_id = lce_req.header.lru_way_id;
          mshr_n.uc_req_size = lce_req.header.uc_size;
          mshr_n.data_length = lce_req.header.data_length;
        end else if (decoded_inst_i.popq_sel == e_src_q_sel_lce_resp) begin
          mshr_n.lce_id = lce_resp.header.src_id;
          mshr_n.paddr = lce_resp.header.addr;
          mshr_n.data_length = lce_resp.header.data_length;
        end else if (decoded_inst_i.popq_sel == e_src_q_sel_mem_resp) begin
          mshr_n.lce_id = mem_resp.header.payload.lce_id;
          mshr_n.paddr = mem_resp.header.addr;
          mshr_n.next_coh_state = mem_resp.header.payload.state;
          // TODO: should MSHR capture mem response size (need conversion)?
          //mshr_n.data_length = mem_resp.header.size;
          mshr_n.way_id = mem_resp.header.payload.way_id;
        end else if (decoded_inst_i.popq_sel == e_src_q_sel_pending) begin
        end
      end

      // Overrides from defaults - GAD
      if (decoded_inst_i.gad_v) begin
        mshr_n.way_id = gad_req_addr_way_i;
        mshr_n.owner_lce_id = gad_owner_lce_i;
        mshr_n.owner_way_id = gad_owner_lce_way_i;
      end

      // Overrides from defaults - Directory
      if (dir_lru_v_i) begin
        mshr_n.lru_paddr = dir_lru_addr_i;
      end

      // Flags
      case (decoded_inst_i.rqf_sel)
        e_rqf_lce_req: begin
          mshr_n.flags[e_flag_sel_rqf] = lce_req_rqf;
        end
        e_rqf_pending: begin
          mshr_n.flags[e_flag_sel_rqf] = '0; // TODO: v2
        end
        e_rqf_src_a: begin
          mshr_n.flags[e_flag_sel_rqf] = src_a_i[0];
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
        e_ucf_src_a: begin
          mshr_n.flags[e_flag_sel_ucf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_ucf] = '0;
        end
      endcase

      case (decoded_inst_i.nerf_sel)
        e_nerf_lce_req: begin
          mshr_n.flags[e_flag_sel_nerf] = lce_req_nerf;
        end
        e_nerf_pending: begin
          mshr_n.flags[e_flag_sel_nerf] = '0; // TODO: v2
        end
        e_nerf_src_a: begin
          mshr_n.flags[e_flag_sel_nerf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_nerf] = '0;
        end
      endcase

      case (decoded_inst_i.ldf_sel)
        e_ldf_lce_req: begin
          mshr_n.flags[e_flag_sel_ldf] = lce_req_ldf;
        end
        e_ldf_pending: begin
          mshr_n.flags[e_flag_sel_ldf] = '0; // TODO: v2
        end
        e_ldf_src_a: begin
          mshr_n.flags[e_flag_sel_ldf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_ldf] = '0;
        end
      endcase

      case (decoded_inst_i.nwbf_sel)
        e_nwbf_lce_resp: begin
          mshr_n.flags[e_flag_sel_nwbf] = lce_resp_nwbf;
        end
        e_nwbf_src_a: begin
          mshr_n.flags[e_flag_sel_nwbf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_nwbf] = '0;
        end
      endcase

      case (decoded_inst_i.tf_sel)
        e_tf_logic: begin
          mshr_n.flags[e_flag_sel_tf] = gad_transfer_flag_i;
        end
        e_tf_src_a: begin
          mshr_n.flags[e_flag_sel_tf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_tf] = '0;
        end
      endcase

      case (decoded_inst_i.pf_sel)
        e_pf_logic: begin
          mshr_n.flags[e_flag_sel_pf] = pending_i; // RDP instruction
        end
        e_pf_src_a: begin
          mshr_n.flags[e_flag_sel_pf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_pf] = '0;
        end
      endcase

      case (decoded_inst_i.rf_sel)
        e_rf_logic: begin
          mshr_n.flags[e_flag_sel_rf] = gad_replacement_flag_i;
        end
        e_rf_src_a: begin
          mshr_n.flags[e_flag_sel_rf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_rf] = '0;
        end
      endcase

      case (decoded_inst_i.uf_sel)
        e_uf_logic: begin
          mshr_n.flags[e_flag_sel_uf] = gad_upgrade_flag_i;
        end
        e_uf_src_a: begin
          mshr_n.flags[e_flag_sel_uf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_uf] = '0;
        end
      endcase

      case (decoded_inst_i.if_sel)
        e_if_logic: begin
          mshr_n.flags[e_flag_sel_if] = gad_invalidate_flag_i;
        end
        e_if_src_a: begin
          mshr_n.flags[e_flag_sel_if] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_if] = '0;
        end
      endcase

      case (decoded_inst_i.cf_sel)
        e_cf_logic: begin
          mshr_n.flags[e_flag_sel_cf] = gad_cached_flag_i;
        end
        e_cf_src_a: begin
          mshr_n.flags[e_flag_sel_cf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cf] = '0;
        end
      endcase

      case (decoded_inst_i.cef_sel)
        e_cef_logic: begin
          mshr_n.flags[e_flag_sel_cef] = gad_cached_exclusive_flag_i;
        end
        e_cef_src_a: begin
          mshr_n.flags[e_flag_sel_cef] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cef] = '0;
        end
      endcase

      case (decoded_inst_i.cof_sel)
        e_cof_logic: begin
          mshr_n.flags[e_flag_sel_cof] = gad_cached_owned_flag_i;
        end
        e_cof_src_a: begin
          mshr_n.flags[e_flag_sel_cof] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cof] = '0;
        end
      endcase

      case (decoded_inst_i.cdf_sel)
        e_cdf_logic: begin
          mshr_n.flags[e_flag_sel_cdf] = gad_cached_dirty_flag_i;
        end
        e_cdf_src_a: begin
          mshr_n.flags[e_flag_sel_cdf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_cdf] = '0;
        end
      endcase

      // TODO: bug: directory outputs LRU info after instruction has been executed
      case (decoded_inst_i.lef_sel)
        e_lef_logic: begin
          mshr_n.flags[e_flag_sel_lef] = dir_lru_cached_excl_i;
        end
        e_lef_src_a: begin
          mshr_n.flags[e_flag_sel_lef] = src_a_i[0];
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
        e_sf_src_a: begin
          mshr_n.flags[e_flag_sel_sf] = src_a_i[0];
        end
        default: begin
          mshr_n.flags[e_flag_sel_sf] = '0;
        end
      endcase

      // TODO: writing flags may need to stall with something?
      if (write_all_flags) begin
        mshr_n.flags = src_a_i[0+:`bp_cce_inst_num_flags];
      end

    end

  end



endmodule
