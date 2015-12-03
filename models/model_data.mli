module type Model_data = sig
  (* list of triangles vertices. Calling [List.iter GlDraw.vertex3 vertex_list] is the idea. *)
  val vertex_list  : ((float * float * float) * (float * float * float)) list
end
