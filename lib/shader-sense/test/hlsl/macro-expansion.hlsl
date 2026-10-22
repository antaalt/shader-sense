#define CBUFFER(Type, Name, Slot)                                              \
  ConstantBuffer<Type> Name : register(b##Slot, space0)

#define STRUCT(Name)                                                           \
  struct Name##Data {                                                          \
    float4 value;                                                              \
  }

#define TEX2D(Name, Slot)                                                      \
  Texture2D Name : register(t##Slot);                                          \
  SamplerState Name##Sampler : register(s##Slot)

#define SLOT0 0
#define CBUFFER_SLOT0(Type, Name) CBUFFER(Type, Name, SLOT0)

struct Material {
  float roughness;
  float metallic;
};

CBUFFER(Material, gMaterial, 0);
STRUCT(Vertex);
TEX2D(gAlbedo, 0);
CBUFFER_SLOT0(Material, gNested);
CBUFFER(Material, gBatch, 1);

// textDocument/definition request test
float4 PSMain() : SV_TARGET {
  // gMaterial is not resolved
  float r = gMaterial.roughness;
  float m = gMaterial.metallic;

  // VertexData type resolves but goes to line 0
  // v.value resolves but goes to line 1
  VertexData v;
  float4 val = v.value;

  // gAlbedo & gAlbedoSampler is not resolved
  gAlbedo.Sample(gAlbedoSampler, float2(0, 0));

  // gFallback is correctly resolves to definition at line 27
  // gFallback.roughness is not resolved (not related to macro expansion I guess)
  float x = gFallback.roughness;

  // gBatch is correctly unresolved with use of undeclared identifier
  float y = gBatch.roughness;

  return 0.xxxx;
}