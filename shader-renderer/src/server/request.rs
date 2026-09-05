use log::info;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use shader_sense::shader::ShaderStage;

use crate::{
    renderer::{shader::Shader, Renderer},
    server::error::ServerError,
};

pub trait Request {
    type Params: DeserializeOwned + Serialize + Send + Sync + 'static;
    type Result: DeserializeOwned + Serialize + Send + Sync + 'static;
    const METHOD: &'static str;

    fn handle_request(
        renderer: &mut Renderer,
        params: Self::Params,
    ) -> Result<Self::Result, ServerError>;
}

/// Dispatch an incoming request to the [`Request`] implementation matching its method.
///
/// Every listed type gets its params deserialized, its handler called and its result
/// sent back as a response, so adding a request only means adding it to the list.
macro_rules! dispatch_request {
    ($self:expr, $request_id:expr, $request:expr, [$($request_type:ty),* $(,)?]) => {{
        let request = $request;
        match request.method.as_str() {
            $(
                <$request_type as $crate::server::request::Request>::METHOD => {
                    let params = serde_json::from_value::<
                        <$request_type as $crate::server::request::Request>::Params,
                    >(request.params)?;
                    let result = <$request_type as $crate::server::request::Request>::handle_request(
                        &mut $self.renderer,
                        params,
                    )?;
                    $self
                        .connection
                        .send_response::<$request_type>($request_id, result);
                    Ok(())
                }
            )*
            method => Err($crate::server::error::ServerError::InternalError(format!(
                "Unhandled request method: {}",
                method
            ))),
        }
    }};
}
pub(crate) use dispatch_request;

#[derive(Debug)]
pub enum ShutdownRequest {}

impl Request for ShutdownRequest {
    type Params = ();
    type Result = ();
    const METHOD: &'static str = "shutdown";

    fn handle_request(_: &mut Renderer, _: ()) -> Result<(), ServerError> {
        Ok(())
    }
}

#[derive(Debug)]
pub enum RenderRequest {}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RenderRequestResult {
    #[serde(with = "base64_bytes")]
    pub data: Vec<u8>, // For now, base64 encoded raw image. Should use shared memory instead.
}

impl Request for RenderRequest {
    type Params = ();
    type Result = RenderRequestResult;
    const METHOD: &'static str = "renderer/render";

    fn handle_request(renderer: &mut Renderer, _: ()) -> Result<RenderRequestResult, ServerError> {
        let data = renderer.render()?;
        Ok(RenderRequestResult { data })
    }
}

pub struct UpdateShaderRequest {}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdateShaderRequestParams {
    pub shader_stage: ShaderStage,
    pub shader: Option<Shader>, // set or unset
}

impl Request for UpdateShaderRequest {
    type Params = UpdateShaderRequestParams;
    type Result = (); // Nothing to return except a potential compilation error
    const METHOD: &'static str = "renderer/updateShader";

    fn handle_request(
        renderer: &mut Renderer,
        params: Self::Params,
    ) -> Result<Self::Result, ServerError> {
        if let Some(shader) = params.shader {
            // The stage is carried by both the params & the shader, so reject a shader
            // that would not end up bound to the slot the client asked for.
            if shader.stage() != params.shader_stage {
                return Err(ServerError::InternalError(format!(
                    "Shader stage {:?} does not match the updated stage {:?}",
                    shader.stage(),
                    params.shader_stage
                )));
            }
            info!(
                "Set shader stage {:?} with entry point {} as {:?}",
                shader.stage(),
                shader.entry_point(),
                shader.shading_language()
            );
            renderer.set_shader(shader)?;
        } else {
            renderer.remove_shader(params.shader_stage);
        }
        Ok(())
    }
}
