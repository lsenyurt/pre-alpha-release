
module bp_uce
  import bp_common_pkg::*;
  import bp_common_aviary_pkg::*;
  import bp_cce_pkg::*;
  import bp_common_cfg_link_pkg::*;
  import bp_me_pkg::*;
  #(parameter bp_params_e bp_params_p = e_bp_inv_cfg
    `declare_bp_proc_params(bp_params_p)

    `declare_bp_me_if_widths(paddr_width_p, cce_block_width_p, lce_id_width_p, lce_assoc_p)
    , localparam cache_service_width_lp = `bp_cache_service_width(paddr_width_p)

    , localparam data_mem_pkt_width_lp = `bp_be_dcache_lce_data_mem_pkt_width(lce_sets_p, lce_assoc_p, cce_block_width_p)
    , localparam tag_mem_pkt_width_lp = `bp_be_dcache_lce_tag_mem_pkt_width(lce_sets_p, lce_assoc_p, ptag_width_p)
    , localparam stat_mem_pkt_width_lp = `bp_be_dcache_lce_stat_mem_pkt_width(lce_sets_p, lce_assoc_p)
    )
   (input clk_i
    , input reset_i

    , input [cache_service_width_lp-1:0] cache_service_i
    , input cache_service_v_i
    , output cache_service_ready_o

    , output [tag_mem_pkt_width_lp-1:0] tag_mem_pkt_o
    , output [data_mem_pkt_width_lp-1:0] data_mem_pkt_o
    , output [stat_mem_pkt_width_lp-1:0] stat_mem_pkt_o
    , output logic cache_fill_v_o
    , input cache_fill_yumi_i

    , output [cce_mem_msg_width_lp-1:0] mem_cmd_o
    , output logic mem_cmd_v_o
    , input mem_cmd_ready_i

    , input [cce_mem_msg_width_lp-1:0] mem_resp_i
    , input mem_resp_v_i
    , output logic mem_resp_yumi_o
    );

  `declare_bp_me_if(paddr_width_p, cce_block_width_p, lce_id_width_p, lce_assoc_p);
  bp_cce_mem_msg_s mem_cmd_cast_o, mem_resp_cast_i;
  assign mem_cmd_o = mem_cmd_cast_o;
  assign mem_resp_cast_i = mem_resp_i;

  `declare_bp_be_dcache_lce_stat_mem_pkt_s(lce_sets_p, lce_assoc_p);
  `declare_bp_be_dcache_lce_tag_mem_pkt_s(lce_sets_p, lce_assoc_p, ptag_width_lp);
  `declare_bp_be_dcache_lce_data_mem_pkt_s(lce_sets_p, lce_assoc_p, dword_width_p*lce_assoc_p);
  bp_be_dcache_lce_stat_mem_pkt_s stat_mem_pkt_cast_o;
  bp_be_dcache_lce_tag_mem_pkt_s tag_mem_pkt_cast_o;
  bp_be_dcache_lce_data_mem_pkt_s data_mem_pkt_cast_o;
  assign stat_mem_pkt_o = stat_mem_pkt_cast_o;
  assign tag_mem_pkt_o = tag_mem_pkt_cast_o;
  assign data_mem_pkt_o = data_mem_pkt_cast_o;

  enum logic [1:0] {e_ready, e_writeback, e_write_request, e_read_request} state_n, state_r;
  wire is_ready         = (state_r == e_ready);
  wire is_writeback     = (state_r == e_writeback);
  wire is_write_request = (state_r == e_write_request);
  wire is_read_request  = (state_r == e_read_request);

  `declare_bp_cache_service_s(paddr_width_p);
  bp_cache_service_s cache_service_cast_i;
  assign cache_service_cast_i = cache_service_i;

  wire miss_load_li = cache_service_v_li & cache_service_cast_i.msg_type inside {e_miss_load};
  wire miss_store_li = cache_service_v_li & cache_service_cast_i.msg_type inside {e_miss_store};
  wire miss_li = cache_service_v_li & (miss_load_li | miss_store_li);
  wire uc_store_li = cache_service_v_li & cache_service_cast_i.msg_type inside {e_uc_store};
  wire wt_store_li = cache_service_v_li & cache_service_cast_i.msg_type inside {e_wt_store};
  wire uc_load_li = cache_service_v_li & cache_service_cast_i.msg_type inside {e_uc_load};
  wire block_read_li = cache_service_v_li & cache_service_cast_i.msg_type inside {e_block_read};

  always_comb
    unique case (state_r)
      e_ready        : state_n = miss_li
                                 ? cache_service_cast_i.dirty 
                                   ? e_writeback 
                                   : e_read_request 
                                 : e_ready;
      e_writeback    : state_n = block_read_li ? e_write_request : e_writeback;
      e_write_request: state_n = mem_resp_yumi_o ? e_read_request : e_ready;
      e_read_request : state_n = cache_service_yumi_lo ? e_ready : e_read_request;
      default: state_n = e_ready;
    endcase

  always_comb
    begin
      stat_mem_pkt_cast_o = '0;
      tag_mem_pkt_cast_o = '0;
      data_mem_pkt_cast_o = '0;
      cache_fill_v_o = '0;

      mem_cmd_cast_o = '0;
      mem_cmd_v_o = '0;

      mem_resp_yumi_o = '0;

      state_n = state_r;

      cache_service_ready_o = '0;

      unique case (state_r)
        e_ready: 
          if (miss_li & ~cache_service_cast_i.dirty)
            begin
              mem_cmd_v_o = mem_cmd_ready_o;
              mem_cmd_cast_o.msg_type = miss_load_li ? e_cce_mem_rd : e_cce_mem_wr;
              mem_cmd_cast_o.addr = cache_service_cast_i.addr;
              mem_cmd_cast_o.size = e_mem_size_64;
              mem_cmd_cast_o.way_id = cache_service_cast_i.repl_way;

              state_n = mem_cmd_v_o ? e_read_request : e_ready;
            end
          else if (miss_li & cache_service_cast_i.dirty)
            begin
              // TODO:
              state_n = block_read_li ? e_write_request : e_writeback;
            end


    end


  always_ff @(posedge clk_i)
    if (reset_i)
      state_r <= e_ready;
    else
      state_r <= state_n;

  //synopsys translate_on
  always_ff @(negedge clk_i)
    begin
      if (cache_service_v_li)
        begin
          assert (~wt_store_li)
            $error("Unsupported op: wt store");
          assert (~block_read_li || is_writeback)
            $error("Block read response received outside of writeback");
        end
    end
  //synopsys translate_off

endmodule

