use shader_sense::{
    shader::{ShaderStage, ShaderStageMask},
    symbols::{
        symbol_list::ShaderSymbolList,
        symbols::{
            HlslRequirementParameter, RequirementParameter, ShaderEnumValue, ShaderMember,
            ShaderParameter, ShaderSignature, ShaderSymbol, ShaderSymbolData,
            ShaderSymbolIntrinsic, ShaderSymbolMode,
        },
    },
};

use super::HlslIntrinsicParser;

impl HlslIntrinsicParser {
    pub fn add_raytracing(&self, symbols: &mut ShaderSymbolList) {
        self.add_raytracing_types(symbols);
        self.add_raytracing_intrinsics(symbols);
        self.add_raytracing_enum(symbols);
    }
    fn add_raytracing_types(&self, symbols: &mut ShaderSymbolList) {
        symbols.types.push(ShaderSymbol {
            label: "RaytracingAccelerationStructure".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A resource type that can be declared in HLSL and passed into TraceRay to indicate the top-level acceleration resource built using BuildRaytracingAccelerationStructure. It is bound as a raw buffer SRV in a descriptor table or root descriptor SRV.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/raytracingaccelerationstructure".into())
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::ANY_HIT),
                ..Default::default()
            })),
            data: ShaderSymbolData::Types { constructors: vec![] },
        });
        symbols.types.push(ShaderSymbol {
            label: "RayDesc".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Passed to the TraceRay function to define the origin, direction, and extents of the ray.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/raydesc".into())
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStage::raytracing()),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![ShaderMember {
                    context: "RayDesc".into(),
                    parameters: ShaderParameter {
                        ty: "float3".into(), 
                        label: "Origin".into(), 
                        count: None,
                        description: "The origin of the ray.".into(), 
                        range: None,
                        modifier: None
                    }
                },
                ShaderMember {
                    context: "RayDesc".into(),
                    parameters: ShaderParameter {
                        ty: "float".into(), 
                        label: "TMin".into(), 
                        count: None,
                        description: "The minimum extent of the ray.".into(), 
                        range: None,
                        modifier: None
                    }
                },
                ShaderMember {
                    context: "RayDesc".into(),
                    parameters: ShaderParameter {
                        ty: "float3".into(), 
                        label: "Direction".into(), 
                        count: None,
                        description: "The direction of the ray.".into(), 
                        range: None,
                        modifier: None
                    }
                },
                ShaderMember {
                    context: "RayDesc".into(),
                    parameters: ShaderParameter {
                        ty: "float".into(), 
                        label: "TMax".into(), 
                        count: None,
                        description: "The maximum extent of the ray.".into(), 
                        range: None,
                        modifier: None
                    }
                }],
                methods: vec![]
            },
        });
        symbols.types.push(ShaderSymbol {
            label: "BuiltInTriangleIntersectionAttributes".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "A structure declared in HLSL to represent hit attributes for fixed-function triangle intersection or axis-aligned bounding box for procedural primitive intersection.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/intersection-attributes".into())
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::ANY_HIT),
                ..Default::default()
            })),
            data: ShaderSymbolData::Struct {
                constructors: vec![],
                members: vec![ShaderMember {
                    context: "BuiltInTriangleIntersectionAttributes".into(),
                    parameters: ShaderParameter {
                        ty: "float2".into(), 
                        label: "barycentrics".into(), 
                        count: None,
                        description: "Any hit and closest hit shaders invoked using fixed-function triangle intersection must use this structure for hit attributes. Given attributes a0, a1 and a2 for the 3 vertices of a triangle, barycentrics.x is the weight for a1 and barycentrics.y is the weight for a2. For example, the app can interpolate by doing: a = a0 + barycentrics.x * (a1-a0) + barycentrics.y* (a2 - a0).".into(), 
                        range: None,
                        modifier: None
                    }
                }],
                methods: vec![]
            },
        });
    }
    fn add_raytracing_intrinsics(&self, symbols: &mut ShaderSymbolList) {
        symbols.functions.push(ShaderSymbol {
            label: "AcceptHitAndEndSearch".into(),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::ANY_HIT),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                    ShaderSignature {
                        returnType: "void".into(), 
                        description: "".into(), 
                        parameters: vec![],
                    }
                ]
            },
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Used in an any hit shader to commit the current hit and then stop searching for more hits for the ray. If there is an intersection shader running, it's execution stops. Execution passes to the closest hit shader, if enabled, with the closest hit recorded so far.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/accepthitandendsearch-function".into())
            )),
        });
        symbols.functions.push(ShaderSymbol {
            label: "CallShader".into(),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::ANY_HIT | ShaderStageMask::CLOSEST_HIT | ShaderStageMask::MISS | ShaderStageMask::RAY_GENERATION),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                    ShaderSignature {
                        returnType: "void".into(), 
                        description: "".into(), 
                        parameters: vec![ShaderParameter {
                            ty: "uint".into(), 
                            label: "ShaderIndex".into(), 
                            count: None,
                            description: "An unsigned integer representing the index into the callable shader table specified in the call to DispatchRays.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "T".into(), 
                            label: "Parameter".into(), 
                            count: None,
                            description: "The user-defined parameters to pass to the callable shader. This parameter structure must match the parameter structure used in the callable shader pointed to in the shader table.".into(), 
                            range: None,
                            modifier: Some("inout".into())
                        }],
                    }
                ]
            },
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Invokes another shader from within a shader.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/callshader-function".into())
            )),
        });
        symbols.functions.push(ShaderSymbol {
            label: "IgnoreHit".into(),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::ANY_HIT),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                    ShaderSignature {
                        returnType: "void".into(), 
                        description: "".into(), 
                        parameters: vec![],
                    }
                ]
            },
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Called from an any hit shader to reject the hit and end the shader. The hit search continues on without committing the distance and attributes for the current hit. The ReportHit call in the intersection shader, if there is one, will return false. Any modifications made to the ray payload up to this point in the any hit shader are preserved.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/ignorehit-function".into())
            )),
        });
        symbols.functions.push(ShaderSymbol {
            label: "ReportHit".into(),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::INTERSECT),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                    ShaderSignature {
                        returnType: "bool".into(), 
                        description: "bool True if the hit was accepted. A hit is rejected if THit is outside the current ray interval, or the any hit shader calls IgnoreHit. The current ray interval is defined by RayTMin and RayTCurrent.".into(), 
                        parameters: vec![ShaderParameter {
                            ty: "float".into(), 
                            label: "THit".into(), 
                            count: None,
                            description: "A float value specifying the parametric distance of the intersection..".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "uint".into(), 
                            label: "HitKind".into(), 
                            count: None,
                            description: "An unsigned integer that identifies the type of hit that occurred. This is a user-specified value in the range of 0-127. The value can be read by any hit or closest hit shaders with the HitKind intrinsic.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "T".into(), 
                            label: "Attributes".into(), 
                            count: None,
                            description: "The user-defined Intersection Attribute Structure structure specifying the intersection attributes.".into(), 
                            range: None,
                            modifier: None
                        }],
                    }
                ]
            },
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Called by an intersection shader to report a ray intersection.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/reporthit-function".into())
            )),
        });
        symbols.functions.push(ShaderSymbol {
            label: "TraceRay".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Sends a ray into a search for hits in an acceleration structure.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/traceray-function".into())
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStageMask::CLOSEST_HIT | ShaderStageMask::MISS | ShaderStageMask::RAY_GENERATION),
                ..Default::default()
            })),
            data: ShaderSymbolData::Functions { signatures: vec![
                    ShaderSignature {
                        returnType: "void".into(), 
                        description: "".into(), 
                        parameters: vec![ShaderParameter {
                            ty: "RaytracingAccelerationStructure".into(), 
                            label: "AccelerationStructure".into(), 
                            count: None,
                            description: "The top-level acceleration structure to use. Specifying a NULL acceleration structure forces a miss.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "uint".into(), 
                            label: "RayFlags".into(), 
                            count: None,
                            description: "Valid combination of ray_flag values. Only defined ray flags are propagated by the system, i.e. are visible to the RayFlags shader intrinsic.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "uint".into(), 
                            label: "InstanceInclusionMask".into(), 
                            count: None,
                            description: "An unsigned integer, the bottom 8 bits of which are used to include or reject geometry instances based on the InstanceMask in each instance.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "uint".into(), 
                            label: "RayContributionToHitGroupIndex".into(), 
                            count: None,
                            description: "An unsigned integer specifying the offset to add into addressing calculations within shader tables for hit group indexing. Only the bottom 4 bits of this value are used.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "uint".into(), 
                            label: "MultiplierForGeometryContributionToHitGroupIndex".into(), 
                            count: None,
                            description: "An unsigned integer specifying the stride to multiply by GeometryContributionToHitGroupIndex, which is just the 0 based index the geometry was supplied by the app into its bottom-level acceleration structure. Only the bottom 16 bits of this multiplier value are used.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "RayDesc".into(), 
                            label: "Ray".into(), 
                            count: None,
                            description: "A RayDesc representing the ray to be traced.".into(), 
                            range: None,
                            modifier: None
                        },
                        ShaderParameter {
                            ty: "T".into(), 
                            label: "Payload".into(), 
                            count: None,
                            description: "A user defined ray payload accessed both for both input and output by shaders invoked during raytracing. After TraceRay completes, the caller can access the payload as well.".into(), 
                            range: None,
                            modifier: Some("inout".into())
                        }],
                    }
                ]
            },
        });
    }
    fn add_raytracing_enum(&self, symbols: &mut ShaderSymbolList) {
        symbols.types.push(ShaderSymbol {
            label: "RAY_FLAG".into(),
            mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                "Flags passed to the TraceRay function to override transparency, culling, and early-out behavior.".into(), 
                Some("https://learn.microsoft.com/en-us/windows/win32/direct3d12/ray_flag".into())
            )),
            requirement: Some(RequirementParameter::Hlsl(HlslRequirementParameter {
                stages: Some(ShaderStage::raytracing()),
                ..Default::default()
            })),
            data: ShaderSymbolData::Enum { values: vec![
                ShaderEnumValue {
                    label: "RAY_FLAG_NONE".into(), 
                    description: "No options selected".into(), 
                    value: Some("0x00".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_OPAQUE".into(), 
                    description: "All ray-primitive intersections encountered in a raytrace are treated as opaque. So no any hit shaders will be executed regardless of whether or not the hit geometry specifies D3D12_RAYTRACING_GEOMETRY_FLAG_OPAQUE, and regardless of the instance flags on the instance that was hit.".into(), 
                    value: Some("0x01".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_FORCE_NON_OPAQUE".into(), 
                    description: "All ray-primitive intersections encountered in a raytrace are treated as non-opaque. So any hit shaders, if present, will be executed regardless of whether or not the hit geometry specifies D3D12_RAYTRACING_GEOMETRY_FLAG_OPAQUE, and regardless of the instance flags on the instance that was hit. This flag is mutually exclusive with RAY_FLAG_FORCE_OPAQUE, RAY_FLAG_CULL_OPAQUE and RAY_FLAG_CULL_NON_OPAQUE".into(), 
                    value: Some("0x02".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH".into(), 
                    description: "The first ray-primitive intersection encountered in a raytrace automatically causes AcceptHitAndEndSearch to be called immediately after the any hit shader, including if there is no any hit shader.".into(), 
                    value: Some("0x04".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_SKIP_CLOSEST_HIT_SHADER".into(), 
                    description: "Even if at least one hit has been committed, and the hit group for the closest hit contains a closest hit shader, skip execution of that shader.".into(), 
                    value: Some("0x08".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_CULL_BACK_FACING_TRIANGLES".into(), 
                    description: "Enables culling of back facing triangles. See D3D12_RAYTRACING_INSTANCE_FLAGS for selecting which triangles are back facing, per-instance.".into(), 
                    value: Some("0x10".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_CULL_FRONT_FACING_TRIANGLES".into(), 
                    description: "Enables culling of front facing triangles. See D3D12_RAYTRACING_INSTANCE_FLAGS for selecting which triangles are back facing, per-instance.".into(), 
                    value: Some("0x20".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_CULL_OPAQUE".into(), 
                    description: "Culls all primitives that are considered opaque based on their geometry and instance flags.".into(), 
                    value: Some("0x40".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_CULL_NON_OPAQUE".into(), 
                    description: "Culls all primitives that are considered non-opaque based on their geometry and instance flags.".into(), 
                    value: Some("0x80".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_SKIP_TRIANGLES".into(), 
                    description: "Culls all triangles.".into(), 
                    value: Some("0x100".into()), 
                    range: None
                },
                ShaderEnumValue {
                    label: "RAY_FLAG_SKIP_PROCEDURAL_PRIMITIVES".into(), 
                    description: "Culls all procedural primitives.".into(), 
                    value: Some("0x200".into()), 
                    range: None
                }
            ]},
        });
    }
}
