(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open OUnit
open Network_utils

module OVS_Cli_test = struct
	include Ovs.Cli
	let vsctl_output = ref []
	let vsctl ?(log=true) args =
		vsctl_output := args ;
		String.concat " " args
end

(* Test OVS.create_bridge for setting mcast_snooping_enable to OVS when carried igmp_snooping value *)
let test_create_bridge () = 
	let module Ovs = Ovs.Make(OVS_Cli_test) in
	let bridge = "xapi1" in
	let expect = "mcast_snooping_enable=true" in
	ignore (Ovs.create_bridge ?mac:None ~fail_mode:"test_mode" ?external_id:None ?disable_in_band:None ?igmp_snooping:(Some true)
			None (Some false) bridge);

	assert_bool "Missing IGMP Snooping toggle!"
		(List.exists
			(fun s -> (s = expect))
			!OVS_Cli_test.vsctl_output)

(* Test OVS.get_mcast_snooping_enable to form the correct parameter to query OVS *)
let test_get_mcast_snooping_enable () = 
	let module Ovs = Ovs.Make(OVS_Cli_test) in
	let bridge = "xapi1" in
	let expect = "-- get bridge xapi1 mcast_snooping_enable" in
	ignore (Ovs.get_mcast_snooping_enable bridge);

	assert_bool "Incorrect input format!"
		((String.concat " " !OVS_Cli_test.vsctl_output) = expect)

(* Test the function form correct format to invoke the script to inject IGMP Query *)
let test_inject_igmp_query () =
	Network_utils.in_unit_test := true;
	let bridge = "xapi1" in
	let expect = "/usr/libexec/xenopsd/igmp_query_injector.py --detach --max-resp-time 5000 bridge xapi1" in
	ignore (Ovs.inject_igmp_query bridge);

	assert_bool "Incorrect script call format!"
	(!Network_utils.unit_test_call_script = expect)

let suite =
	"igmp_tests" >:::
		[
			"test_create_bridge" >:: test_create_bridge;
			"test_get_mcast_snooping_enable" >:: test_get_mcast_snooping_enable;
			"test_inject_igmp_query" >:: test_inject_igmp_query;
		]
