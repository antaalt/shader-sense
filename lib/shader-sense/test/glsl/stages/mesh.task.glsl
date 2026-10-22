#version 460
#extension GL_EXT_mesh_shader : require

layout(local_size_x = 32) in;

struct TaskData {
    uint meshletID;
};

taskPayloadSharedEXT TaskData payload;

void TSMain()
{
    payload.meshletID = gl_LocalInvocationID.x;
	EmitMeshTasksEXT(3, 1, 1);
}