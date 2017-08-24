(*
 * Copyright (C) 2017 Citrix Systems Inc.
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
open Stdext
open Network_utils

module OVS_Cli_test = struct
  include Ovs.Cli
  let vsctl_output = ref []
  let vsctl ?(log=true) args =
    vsctl_output := args ;
    String.concat " " args
end

module Ovs = Ovs.Make(OVS_Cli_test)

let elem_in_list elem lst =
  List.exists (fun s -> (Xstringext.String.(strip isspace s) = elem)) lst

let test_enable_ipv6_mcast_snooping_default_false () =
  let answer = "other_config:enable-ipv6-mcast-snooping=false" in
  Ovs.create_bridge ~igmp_snooping:true ~fail_mode:"standalone" None (Some true) "bridge0" |> ignore;
  assert_bool "ipv6 mcast snooping default should be disable" (elem_in_list answer !OVS_Cli_test.vsctl_output)

let test_ipv6_mcast_snooping_setable_only_when_igmp_snooping_enabled () =
  let answer = "other_config:enable-ipv6-mcast-snooping=false" in
  Ovs.create_bridge ?igmp_snooping:None ~fail_mode:"standalone" None (Some true) "bridge0" |> ignore;
  assert_bool "ipv6 mcast snooping should be setable only when igmp snooping enabled"
    (not (elem_in_list answer !OVS_Cli_test.vsctl_output))

let test_enable_ipv6_mcast_snooping_true () =
  let origin_val = !Network_utils.enable_ipv6_mcast_snooping in
  Pervasiveext.finally (fun () ->
      Network_utils.enable_ipv6_mcast_snooping := true;
      let answer = "other_config:enable-ipv6-mcast-snooping=true" in
      Ovs.create_bridge ~igmp_snooping:true ~fail_mode:"standalone" None (Some true) "bridge0" |> ignore;
      assert_bool "ipv6 mcast snooping should be enable" (elem_in_list answer !OVS_Cli_test.vsctl_output);
    ) (fun () ->
      Network_utils.enable_ipv6_mcast_snooping := origin_val
    )

let test_mcast_snooping_disable_flood_unregistered_traffic_dafault_true () =
  let answer = "other_config:mcast-snooping-disable-flood-unregistered=true" in
  Ovs.create_bridge ~igmp_snooping:true ~fail_mode:"standalone" None (Some true) "bridge0" |> ignore;
  assert_bool "mcast snooping disable flood unregistered traffic default shoule be true" (elem_in_list answer !OVS_Cli_test.vsctl_output)

let test_mcast_snooping_disable_flood_unregistered_traffic_setable_only_when_igmp_snooping_enabled () =
  let answer = "other_config:mcast-snooping-disable-flood-unregistered=true" in
  Ovs.create_bridge ?igmp_snooping:None ~fail_mode:"standalone" None (Some true) "bridge0" |> ignore;
  assert_bool "mcast snooping disable flood unregistered traffic should be setable only when igmp snooping enabled" (not (elem_in_list answer !OVS_Cli_test.vsctl_output))

let test_mcast_snooping_disable_flood_unregistered_traffic_false () =
  let origin_val = !Network_utils.mcast_snooping_disable_flood_unregistered in
  Pervasiveext.finally (fun () ->
      Network_utils.mcast_snooping_disable_flood_unregistered := false;
      let answer = "other_config:mcast-snooping-disable-flood-unregistered=false" in
      Ovs.create_bridge ~igmp_snooping:true ~fail_mode:"standalone" None (Some true) "bridge0" |> ignore;
      assert_bool "mcast snooping disable flood unregistered traffic false" (elem_in_list answer !OVS_Cli_test.vsctl_output)
    ) (fun () ->
      Network_utils.mcast_snooping_disable_flood_unregistered := origin_val
    )

let suite =
  "ovs" >:::
  [
    "test_enable_ipv6_mcast_snooping_default_false" >:: test_enable_ipv6_mcast_snooping_default_false;
      "test_ipv6_mcast_snooping_setable_only_when_igmp_snooping_enabled" >:: test_ipv6_mcast_snooping_setable_only_when_igmp_snooping_enabled;
      "test_enable_ipv6_mcast_snooping_true" >:: test_enable_ipv6_mcast_snooping_true;
      "test_mcast_snooping_disable_flood_unregistered_traffic_dafault_true" >:: test_mcast_snooping_disable_flood_unregistered_traffic_dafault_true;
      "test_mcast_snooping_disable_flood_unregistered_traffic_setable_only_when_igmp_snooping_enabled" >:: test_mcast_snooping_disable_flood_unregistered_traffic_setable_only_when_igmp_snooping_enabled;
      "test_mcast_snooping_disable_flood_unregistered_traffic_false" >:: test_mcast_snooping_disable_flood_unregistered_traffic_false;
    ]
