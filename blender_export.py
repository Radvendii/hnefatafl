import bpy

filename = bpy.context.blend_data.filepath[:-6] + ".ml"
out = open(filename, 'w')
obj = bpy.data.objects['Export']
s = obj.scale
t = obj.location
mesh = bpy.data.meshes['Export']
vs = mesh.vertices
out.write('open Model_data\n')
out.write('module Data : Model_data = struct\n')
out.write('  let vertex_list =\n')
out.write('    [ ')
first = True
for face in mesh.polygons:
    if not first:
        out.write('\n')
    for vi in face.vertices:
        if not first:
            out.write('    ; ')
        first = False
        l = vs[vi].co
        n = vs[vi].normal
        out.write('(%f, %f, %f), (%f, %f, %f)\n' % (n.x, n.y, -n.z, l.x * s.x + t.x, l.y * s.y + t.y, -(l.z * s.z + t.z)))
out.write('    ]\n')
out.write('end')
