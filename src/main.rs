use std::f32::consts::PI;

use bevy::app::App;
use bevy::DefaultPlugins;
use bevy::ecs::component::Component;
use bevy::ecs::system::lifetimeless::SRes;
use bevy::ecs::system::SystemParamItem;
use bevy::input::mouse::{MouseMotion, MouseWheel};
use bevy::pbr::{AmbientLight, MaterialMeshBundle, MaterialPipeline, PointLightBundle, SpecializedMaterial};
use bevy::prelude::{Assets, Camera, EventReader, Mat3, MaterialPlugin, MouseButton, PerspectiveCameraBundle, PerspectiveProjection, SpawnSceneCommands, Vec2, Vec4, Windows};
use bevy::prelude::AssetServer;
use bevy::prelude::Color;
use bevy::prelude::Commands;
use bevy::prelude::Handle;
use bevy::prelude::Input;
use bevy::prelude::KeyCode;
use bevy::prelude::Mesh;
use bevy::prelude::Msaa;
use bevy::prelude::PointLight;
use bevy::prelude::Quat;
use bevy::prelude::Query;
use bevy::prelude::Res;
use bevy::prelude::ResMut;
use bevy::prelude::Shader;
use bevy::prelude::shape;
use bevy::prelude::Time;
use bevy::prelude::Transform;
use bevy::prelude::Vec3;
use bevy::prelude::With;
use bevy::reflect::TypeUuid;
use bevy::render::mesh::MeshVertexBufferLayout;
use bevy::render::render_asset::{PrepareAssetError, RenderAsset, RenderAssets};
use bevy::render::render_resource::{BindGroup, BindGroupDescriptor, BindGroupEntry, BindGroupLayout, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingType, Buffer, BufferBindingType, BufferInitDescriptor, BufferSize, BufferUsages, RenderPipelineDescriptor, ShaderStages, SpecializedMeshPipelineError};
use bevy::render::render_resource::std140::{AsStd140, Std140};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::{RenderApp, RenderStage};
use bevy::utils::default;
use rand::Rng;

fn main() {
    let mut app = App::new();

    // resources
    app.insert_resource(Msaa { samples: 4 })
        .add_plugins(DefaultPlugins)
        .add_plugin(MaterialPlugin::<CustomMaterial>::default())
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 1.0 / 5.0
        })
        .add_startup_system(setup)
        .add_system(pan_orbit_camera)
        .add_system(disco)
        .add_system(light_orbit);

    // render stages
    app.sub_app_mut(RenderApp)
        .add_system_to_stage(RenderStage::Extract, extract_camera_eye)
        .add_system_to_stage(RenderStage::Prepare, prepare_camera_eye);

    app.run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials2: ResMut<Assets<CustomMaterial>>,
    asset_server: Res<AssetServer>,
) {
    // ground
    commands.spawn_bundle(MaterialMeshBundle {
        mesh: meshes.add(Mesh::from(shape::Plane { size: 3000.0 })),
        material: materials2.add(CustomMaterial {
            color: Color::rgb(0.7, 0.7, 0.7),
            enabled: true,
            ..default()
        }),
        transform: Transform::from_xyz(0.0, 0.0, 0.0),
        ..default()
    });

    // model
    commands.spawn_scene(asset_server.load("models/gltf_test.gltf#Scene-"));

    // cube
    for x in 1..10 {
        for y in 1..10 {
            for z in 1..10 {
                commands.spawn().insert_bundle(MaterialMeshBundle {
                    mesh: meshes.add(Mesh::from(shape::Cube { size: 1.0 })),
                    material: materials2.add(CustomMaterial{
                        color: Color::rgb(1.0, 0.0, 0.0),
                        enabled: true,
                        ..default()
                    }),
                    transform: Transform::from_xyz(0.0 + x as f32 * 2.0, 0.5 + y as f32 * 2.0, 0.0 + z as f32 * 2.0),
                    ..default()
                });
            }
        }
    }

    commands.spawn_bundle(PerspectiveCameraBundle {
        transform: Transform::from_xyz(-10.0, 5.0, -10.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    }).insert(PanOrbitCamera{
        ..default()
    });

    commands.spawn_bundle(PointLightBundle {
        point_light: PointLight {
            color: Color::rgb(0.0, 0.0, 1.0),
            intensity: 750.0,
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(0.0, 10.0, 0.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
}

fn light_orbit(time: Res<Time>, mut query: Query<&mut Transform, With<PointLight>>) {
    for mut lightb in query.iter_mut() {
        lightb.rotate_around(Vec3::ZERO, Quat::from_rotation_z(PI * time.delta_seconds()));
    }
}

fn disco(input: Res<Input<KeyCode>>, mut query: Query<&mut PointLight>) {
    if input.just_pressed(KeyCode::M) {
        for mut light in query.iter_mut() {
            let mut rng = rand::thread_rng();

            light.color = Color::rgb(rng.gen::<f32>(), rng.gen::<f32>(), rng.gen::<f32>());
        }
    }
}

struct ExtractedCameraEye {
    camera_origin: Vec3
}

// extract the camera position into a resource in render world
fn extract_camera_eye(mut commands: Commands, query: Query<(&Transform, &Camera)>) {
    for (transform, _) in query.iter() {
        commands.insert_resource(ExtractedCameraEye {
            camera_origin: transform.translation
        });
    }
}

// write the camera position into the buffer for our shader asset
fn prepare_camera_eye(mut material_assets: ResMut<RenderAssets<CustomMaterial>>, camera_eye: Res<ExtractedCameraEye>, render_queue: Res<RenderQueue>) {
    for material in material_assets.values_mut() {
        material.uniform_data.origin = camera_eye.camera_origin;
        render_queue.write_buffer(
            &material._buffer,
            0,
            material.uniform_data.as_std140().as_bytes(),
        )
    }
}

// This struct is the custom material itself
#[derive(Clone, TypeUuid, Default)]
#[uuid = "4ee9c363-1124-4113-890e-199d81b00281"]
pub struct CustomMaterial {
    color: Color,
    origin: Vec3,
    enabled: bool
}

// This is the struct that will be passed to the shader, using AsStd140 for uniforms
#[derive(Clone, AsStd140)]
struct CustomMaterialUniformData {
    color: Vec4,
    origin: Vec3,
    enabled: f32
}

#[derive(Clone)]
pub struct GpuCustomMaterial {
    _buffer: Buffer,
    uniform_data: CustomMaterialUniformData,
    bind_group: BindGroup,
}

impl RenderAsset for CustomMaterial {
    type ExtractedAsset = CustomMaterial;
    type PreparedAsset = GpuCustomMaterial;
    type Param = (
        SRes<RenderDevice>,
        SRes<MaterialPipeline<Self>>
    );

    fn extract_asset(&self) -> Self::ExtractedAsset {
        self.clone()
    }

    fn prepare_asset(
        extracted_asset: Self::ExtractedAsset,
        (render_device, material_pipeline): &mut SystemParamItem<Self::Param>,
    ) -> Result<Self::PreparedAsset, PrepareAssetError<Self::ExtractedAsset>> {
        // prepare attributes passed to shader
        let uniform_data = CustomMaterialUniformData {
            color: extracted_asset.color.as_linear_rgba_f32().into(),
            origin: extracted_asset.origin,
            enabled: (extracted_asset.enabled as i32) as f32,
        };


        // load data buffer for shader
        let buffer = render_device.create_buffer_with_data(&BufferInitDescriptor {
            contents: uniform_data.as_std140().as_bytes(),
            label: None,
            usage: BufferUsages::UNIFORM | BufferUsages::COPY_DST,
        });

        let bind_group = render_device.create_bind_group(&BindGroupDescriptor {
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: buffer.as_entire_binding(),
                },
            ],
            label: None,
            layout: &material_pipeline.material_layout,
        });

        Ok(GpuCustomMaterial {
            _buffer: buffer,
            uniform_data,
            bind_group,
        })
    }
}

impl SpecializedMaterial for CustomMaterial {
    type Key = ();

    fn key(_: &<CustomMaterial as RenderAsset>::PreparedAsset) -> Self::Key {}

    fn specialize(
        _pipeline: &MaterialPipeline<Self>,
        descriptor: &mut RenderPipelineDescriptor,
        _: Self::Key,
        _layout: &MeshVertexBufferLayout,
    ) -> Result<(), SpecializedMeshPipelineError> {
        descriptor.vertex.entry_point = "main".into();
        descriptor.fragment.as_mut().unwrap().entry_point = "main".into();
        Ok(())
    }

    fn vertex_shader(asset_server: &AssetServer) -> Option<Handle<Shader>> {
        asset_server.watch_for_changes().unwrap();
        Some(asset_server.load("shaders/custom_material.vert"))
    }

    fn fragment_shader(asset_server: &AssetServer) -> Option<Handle<Shader>> {
        asset_server.watch_for_changes().unwrap();
        Some(asset_server.load("shaders/custom_material.frag"))
    }

    fn bind_group(render_asset: &<Self as RenderAsset>::PreparedAsset) -> &BindGroup {
        &render_asset.bind_group
    }

    fn bind_group_layout(render_device: &RenderDevice) -> BindGroupLayout {
        render_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            entries: &[
                // the uniform bind group
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: BufferSize::new(
                            CustomMaterialUniformData::std140_size_static() as u64
                        ),
                    },
                    count: None,
                },
            ],
            label: None,
        })
    }
}


//////////////////////

/// Tags an entity as capable of panning and orbiting.
#[derive(Component)]
struct PanOrbitCamera {
    /// The "focus point" to orbit around. It is automatically updated when panning the camera
    pub focus: Vec3,
    pub radius: f32,
    pub upside_down: bool,
}

impl Default for PanOrbitCamera {
    fn default() -> Self {
        PanOrbitCamera {
            focus: Vec3::ZERO,
            radius: 5.0,
            upside_down: false,
        }
    }
}

/// Pan the camera with middle mouse click, zoom with scroll wheel, orbit with right mouse click.
fn pan_orbit_camera(
    windows: Res<Windows>,
    mut ev_motion: EventReader<MouseMotion>,
    mut ev_scroll: EventReader<MouseWheel>,
    input_mouse: Res<Input<MouseButton>>,
    mut query: Query<(&mut PanOrbitCamera, &mut Transform, &PerspectiveProjection)>,
) {
    // change input mapping for orbit and panning here
    let orbit_button = MouseButton::Right;
    let pan_button = MouseButton::Left;

    let mut pan = Vec2::ZERO;
    let mut rotation_move = Vec2::ZERO;
    let mut scroll = 0.0;
    let mut orbit_button_changed = false;

    if input_mouse.pressed(orbit_button) {
        for ev in ev_motion.iter() {
            rotation_move += ev.delta;
        }
    } else if input_mouse.pressed(pan_button) {
        // Pan only if we're not rotating at the moment
        for ev in ev_motion.iter() {
            pan += ev.delta;
        }
    }
    for ev in ev_scroll.iter() {
        scroll += ev.y;
    }
    if input_mouse.just_released(orbit_button) || input_mouse.just_pressed(orbit_button) {
        orbit_button_changed = true;
    }

    for (mut pan_orbit, mut transform, projection) in query.iter_mut() {
        if orbit_button_changed {
            // only check for upside down when orbiting started or ended this frame
            // if the camera is "upside" down, panning horizontally would be inverted, so invert the input to make it correct
            let up = transform.rotation * Vec3::Y;
            pan_orbit.upside_down = up.y <= 0.0;
        }

        let mut any = false;
        if rotation_move.length_squared() > 0.0 {
            any = true;
            let window = get_primary_window_size(&windows);
            let delta_x = {
                let delta = rotation_move.x / window.x * std::f32::consts::PI * 2.0;
                if pan_orbit.upside_down { -delta } else { delta }
            };
            let delta_y = rotation_move.y / window.y * std::f32::consts::PI;
            let yaw = Quat::from_rotation_y(-delta_x);
            let pitch = Quat::from_rotation_x(-delta_y);
            transform.rotation = yaw * transform.rotation; // rotate around global y axis
            transform.rotation = transform.rotation * pitch; // rotate around local x axis
        } else if pan.length_squared() > 0.0 {
            any = true;
            // make panning distance independent of resolution and FOV,
            let window = get_primary_window_size(&windows);
            pan *= Vec2::new(projection.fov * projection.aspect_ratio, projection.fov) / window;
            // translate by local axes
            let right = transform.rotation * Vec3::X * -pan.x;
            let up = transform.rotation * Vec3::Y * pan.y;
            // make panning proportional to distance away from focus point
            let translation = (right + up) * pan_orbit.radius;
            pan_orbit.focus += translation;
        } else if scroll.abs() > 0.0 {
            any = true;
            pan_orbit.radius -= scroll * pan_orbit.radius * 0.2;
            // dont allow zoom to reach zero or you get stuck
            pan_orbit.radius = f32::max(pan_orbit.radius, 0.05);
        }

        if any {
            // emulating parent/child to make the yaw/y-axis rotation behave like a turntable
            // parent = x and y rotation
            // child = z-offset
            let rot_matrix = Mat3::from_quat(transform.rotation);
            transform.translation = pan_orbit.focus + rot_matrix.mul_vec3(Vec3::new(0.0, 0.0, pan_orbit.radius));
        }
    }
}

fn get_primary_window_size(windows: &Res<Windows>) -> Vec2 {
    let window = windows.get_primary().unwrap();
    let window = Vec2::new(window.width() as f32, window.height() as f32);
    window
}