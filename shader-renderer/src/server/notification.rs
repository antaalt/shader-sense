use log::info;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use shader_sense::shader::ShaderStage;

use crate::{
    renderer::{Renderer, Shader, ShaderSource},
    server::error::ServerError,
};

pub trait Notification {
    type Params: DeserializeOwned + Serialize + Send + Sync + 'static;
    const METHOD: &'static str;

    fn handle_notification(
        renderer: &mut Renderer,
        params: Self::Params,
    ) -> Result<(), ServerError>;
}

/// Dispatch an incoming notification to the [`Notification`] implementation matching
/// its method. Unlike requests, notifications have no response to send back.
macro_rules! dispatch_notification {
    ($self:expr, $notification:expr, [$($notification_type:ty),* $(,)?]) => {{
        let notification = $notification;
        match notification.method.as_str() {
            $(
                <$notification_type as $crate::server::notification::Notification>::METHOD => {
                    let params = serde_json::from_value::<
                        <$notification_type as $crate::server::notification::Notification>::Params,
                    >(notification.params)?;
                    <$notification_type as $crate::server::notification::Notification>::handle_notification(
                        &mut $self.renderer,
                        params,
                    )
                }
            )*
            method => Err($crate::server::error::ServerError::InternalError(format!(
                "Unhandled notification method: {}",
                method
            ))),
        }
    }};
}
pub(crate) use dispatch_notification;

pub struct ErrorNotification {}

#[derive(Serialize, Deserialize)]
pub struct ErrorNotificationParams {
    pub message: String,
}

impl Notification for ErrorNotification {
    type Params = ErrorNotificationParams;

    const METHOD: &'static str = "server/error";

    fn handle_notification(
        _renderer: &mut Renderer,
        params: Self::Params,
    ) -> Result<(), ServerError> {
        Err(ServerError::InternalError(format!(
            "Should not receive error from client, received {}.",
            params.message
        )))
    }
}

pub struct ResizeTargetNotification {}

#[derive(Serialize, Deserialize)]
pub struct ResizeTargetNotificationParams {
    width: u32,
    height: u32,
}

impl Notification for ResizeTargetNotification {
    type Params = ResizeTargetNotificationParams;

    const METHOD: &'static str = "renderer/resize";

    fn handle_notification(
        renderer: &mut Renderer,
        params: Self::Params,
    ) -> Result<(), ServerError> {
        info!("Resizing renderer to {}x{}", params.width, params.height);
        renderer.resize(params.width, params.height);
        Ok(())
    }
}

pub struct UpdateGraphicPipeline {
    depth: bool,
}

pub struct UpdateShaderNotification {}

#[derive(Serialize, Deserialize)]
pub struct UpdateShaderNotificationParams {
    shader_stage: ShaderStage,
    shader: Option<Shader>, // set or unset
}

impl Notification for UpdateShaderNotification {
    type Params = UpdateShaderNotificationParams;

    const METHOD: &'static str = "renderer/updateShader";

    fn handle_notification(
        renderer: &mut Renderer,
        params: Self::Params,
    ) -> Result<(), ServerError> {
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
                "Set shader stage {:?} with entry point {} as {}",
                shader.stage(),
                shader.entry_point(),
                match shader.source() {
                    ShaderSource::Spirv(_) => "Spirv",
                    ShaderSource::Dxil(_) => "Dxil",
                    ShaderSource::Wgsl(_) => "Wgsl",
                    ShaderSource::Glsl(_) => "Glsl",
                }
            );
            renderer.set_shader(shader);
        } else {
            renderer.remove_shader(params.shader_stage);
        }
        Ok(())
    }
}
