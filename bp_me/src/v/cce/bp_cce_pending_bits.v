/**
 *
 * Name:
 *   bp_cce_pending_bits.v
 *
 * Description:
 *   This module contains the pending bits. Pending bits are actually small counters.
 *   The pending bit is unset if the count is 0, and set if the count is > 0.
 *
 *   The pending bits are stored in flops and may be read asynchronously. Write to Read
 *   forwarding is supported. If both clear_i and pending_i are asserted, only the clear
 *   to 0 is processed.
 *
 *   WARNING: the pending bit counters do not saturate and may over/underflow. Be careful!
 *
 *   addr_width_p should be the number of bits used to index the system total number of way
 *   groups. num_way_groups_p is the number of way groups managed by this CCE (or that number
 *   plus one in the event that there is not an even number of way groups per CCE).
 */

module bp_cce_pending_bits
  import bp_common_pkg::*;
  import bp_cce_pkg::*;
  #(parameter num_way_groups_p            = "inv"
    , parameter num_cce_p                 = "inv"
    , parameter addr_width_p              = "inv"

    // Default parameters
    , parameter width_p                   = 2  // pending bit counter width

    // Derived parameters
    , localparam lg_num_way_groups_lp     = `BSG_SAFE_CLOG2(num_way_groups_p)

  )
  (input                                                          clk_i
   , input                                                        reset_i

   , input                                                        w_v_i
   , input [addr_width_p-1:0]                                     w_addr_i
   , input                                                        w_addr_bypass_hash_i
   , input                                                        pending_i
   , input                                                        clear_i

   , input                                                        r_v_i
   , input [addr_width_p-1:0]                                     r_addr_i
   , input                                                        r_addr_bypass_hash_i

   , output logic                                                 pending_o
  );

  // Address to way group hashing
  logic [lg_num_way_groups_lp-1:0] r_wg_lo, w_wg_lo;
  wire [addr_width_p-1:0] r_addr_rev = {<< {r_addr_i}};
  wire [addr_width_p-1:0] w_addr_rev = {<< {w_addr_i}};
  logic [lg_num_way_groups_lp-1:0] r_wg, w_wg;

  bsg_hash_bank
    #(.banks_p(num_cce_p) // number of CCE's to spread way groups over
      ,.width_p(addr_width_p) // width of address input
      )
    r_addr_hash
     (.i(r_addr_rev)
      ,.bank_o()
      ,.index_o(r_wg_lo)
      );

  bsg_hash_bank
    #(.banks_p(num_cce_p) // number of CCE's to spread way groups over
      ,.width_p(addr_width_p) // width of address input
      )
    w_addr_hash
     (.i(w_addr_rev)
      ,.bank_o()
      ,.index_o(w_wg_lo)
      );

  assign r_wg = (r_addr_bypass_hash_i) ? r_addr_i[0+:lg_num_way_groups_lp] : r_wg_lo;
  assign w_wg = (w_addr_bypass_hash_i) ? w_addr_i[0+:lg_num_way_groups_lp] : w_wg_lo;

  // pending bits
  logic [num_way_groups_p-1:0][width_p-1:0] pending_bits_r, pending_bits_n;

  always_ff @(posedge clk_i) begin
    if (reset_i) begin
      pending_bits_r <= '0;
    end else begin
      pending_bits_r <= pending_bits_n;
    end
  end

  always_comb begin
    pending_bits_n = pending_bits_r;
    if (w_v_i) begin
      if (clear_i) begin
        pending_bits_n[w_wg] = '0;
      end
      else begin
        if (pending_i) begin // increment count
          pending_bits_n[w_wg] = pending_bits_r[w_wg] + 'd1;
        end else begin // decrement count
          pending_bits_n[w_wg] = pending_bits_r[w_wg] - 'd1;
        end
      end
    end
  end

  // Pending bit output
  // Normally, the output is determined by the read way group and comes from the flopped values
  // If reading from the same way group that is being written, output the next value
  assign pending_o = (r_v_i & w_v_i & (w_wg == r_wg))
    ? ~(pending_bits_n[r_wg] == 0)
    : ~(pending_bits_r[r_wg] == 0);

endmodule