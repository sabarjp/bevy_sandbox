use std::net::UdpSocket;
use std::time::SystemTime;

use bevy::app::App;
use bevy::DefaultPlugins;
use bevy::ecs::component::Component;
use bevy::ecs::system::lifetimeless::SRes;
use bevy::ecs::system::SystemParamItem;
use bevy::input::mouse::{MouseMotion, MouseWheel};
use bevy::pbr::{AmbientLight, MaterialMeshBundle, MaterialPipeline, PointLightBundle, SpecializedMaterial};
use bevy::prelude::{Assets, Camera, EventReader, Mat3, MaterialPlugin, MouseButton, PerspectiveCameraBundle, PerspectiveProjection, State, SystemSet, Vec2, Vec4, Windows, World};
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
use bevy::reflect::TypeUuid;
use bevy::render::mesh::MeshVertexBufferLayout;
use bevy::render::render_asset::{PrepareAssetError, RenderAsset, RenderAssets};
use bevy::render::render_resource::{BindGroup, BindGroupDescriptor, BindGroupEntry, BindGroupLayout, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingType, Buffer, BufferBindingType, BufferInitDescriptor, BufferSize, BufferUsages, Extent3d, PipelineCache, RenderPipelineDescriptor, ShaderStages, SpecializedMeshPipelineError, StorageTextureAccess, TextureDimension, TextureFormat, TextureUsages, TextureViewDimension};
use bevy::render::render_resource::std140::{AsStd140, Std140};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::{RenderApp, RenderStage};
use bevy::utils::default;
use bevy_renet::renet::{ClientAuthentication, RenetClient, RenetConnectionConfig, RenetServer, ServerAuthentication, ServerConfig, ServerEvent};
use bevy_renet::{RenetClientPlugin, RenetServerPlugin};
use rand::Rng;
use bevy::ecs::schedule::ShouldRun;

#[derive(Debug, PartialEq, Eq)]
enum MultiplayerKind {
    Client,
    Host,
    Local,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum ServerState {
    Running,
    Stopping,
    Stopped
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum ClientState {
    Running,
    Stopping,
    Stopped
}

fn run_if_client(mode: Res<MultiplayerKind>) -> ShouldRun {
    if *mode == MultiplayerKind::Client {
        ShouldRun::Yes
    } else {
        ShouldRun::No
    }
}

fn run_if_host_or_local(mode: Res<MultiplayerKind>) -> ShouldRun {
    if *mode == MultiplayerKind::Host || *mode == MultiplayerKind::Local {
        ShouldRun::Yes
    } else {
        ShouldRun::No
    }
}

fn run_if_host(mode: Res<MultiplayerKind>) -> ShouldRun {
    if *mode == MultiplayerKind::Host {
        ShouldRun::Yes
    } else {
        ShouldRun::No
    }
}

fn main() {
    let mut app = App::new();

    app.add_plugin(RenetServerPlugin);
    app.add_plugin(RenetClientPlugin);

    // default to local hosting
    app.insert_resource(MultiplayerKind::Local);

    app
        .add_state(ServerState::Stopped)
        .add_state(ClientState::Stopped);

    // conditionals
    app.add_system_set(
        SystemSet::new()
            .with_run_criteria(run_if_host)
            .before("input")
            .with_system(send_message_to_client)
            .with_system(receive_message_as_server)
            .with_system(handle_events_as_server)
    );

    app.add_system_set(
        SystemSet::new()
            .with_run_criteria(run_if_client)
            .before("input")
            .with_system(send_message_to_server)
            .with_system(receive_message_as_client)
    );

    app.add_system_set(
        SystemSet::on_enter(ClientState::Running)
            .with_system(prepare_client_socket)
    );

    app.add_system_set(
        SystemSet::on_enter(ClientState::Stopping)
            .with_system(teardown_client_socket)
    );

    app.add_system_set(
        SystemSet::on_enter(ServerState::Running)
            .with_system(host_server)
    );

    app.add_system_set(
        SystemSet::on_enter(ServerState::Stopping)
            .with_system(kill_server)
    );

    // resources
    app
        .insert_resource(Msaa { samples: 4 })
        .add_plugins(DefaultPlugins)
        .add_plugin(MaterialPlugin::<CustomMaterial>::default())
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 1.0 / 5.0
        })
        .add_startup_system(setup)
        .add_startup_system(spawn_player_on_server);

    // input system
    app.add_system_set(
        SystemSet::new()
            .label("input")
            .with_system(pan_orbit_camera)
            .with_system(move_torus)
            .with_system(enter_multiplayer)
    );

    // render stages
    app.sub_app_mut(RenderApp)
        .add_system_to_stage(RenderStage::Extract, extract_camera_eye)
        .add_system_to_stage(RenderStage::Prepare, prepare_camera_eye);

    app.run();
}

fn setup(
    mut commands: Commands,
) {
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

fn spawn_player_on_server(mut commands: Commands, mut meshes: ResMut<Assets<Mesh>>, mut materials: ResMut<Assets<CustomMaterial>>,mode: Res<MultiplayerKind>) {
    if *mode == MultiplayerKind::Host || *mode == MultiplayerKind::Local {
        commands.spawn().insert_bundle(MaterialMeshBundle {
            mesh: meshes.add(Mesh::from(shape::Torus { radius: 2.0, ring_radius: 1.0, subdivisions_segments: 24, subdivisions_sides: 64 })),
            material: materials.add(CustomMaterial{
                color: Color::rgb(1.0, 0.0, 0.0),
                enabled: true,
                ..default()
            }),
            transform: Transform::from_xyz(0.0, 0.5, 0.0),
            ..default()
        }).insert(Player {});
    }
}

#[derive(Component)]
struct Player {}

fn host_server(mut commands: Commands) {
    println!("Starting server...");
    let addr = "127.0.0.1:5000".parse().unwrap();
    let server = RenetServer::new(
        SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap(),
        ServerConfig::new(4, 0, addr, ServerAuthentication::Unsecure),
        RenetConnectionConfig {
            // this can be heavily customized
            ..Default::default()
        },
        UdpSocket::bind(addr).unwrap(),
    ).unwrap();
    commands.insert_resource(server);
}

fn kill_server(mut server: ResMut<RenetServer>, mut commands: Commands, mut server_state: ResMut<State<ServerState>>) {
    println!("Stopping server...");
    server.disconnect_clients();
    commands.remove_resource::<RenetServer>();
    if *server_state.current() != ServerState::Stopped {
        server_state.set(ServerState::Stopped).unwrap();
    }
}

fn prepare_client_socket(mut commands: Commands) {
    println!("Establishing client socket...");
    let mut rng = rand::thread_rng();
    let addr = "127.0.0.1:5000".parse().unwrap();
    let client_id = rng.gen::<u64>();
    let client = RenetClient::new(
        SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap(),
        UdpSocket::bind("127.0.0.1:0").unwrap(),
        client_id,
        RenetConnectionConfig {
            // this can be heavily customized
            ..Default::default()
        },
        ClientAuthentication::Unsecure {
            protocol_id: 0,
            client_id,
            server_addr: addr,
            user_data: None
        }
    ).unwrap();
    commands.insert_resource(client);
}

fn teardown_client_socket(mut client: ResMut<RenetClient>, mut commands: Commands, mut client_state: ResMut<State<ClientState>>) {
    println!("Tearing down client socket...");
    client.disconnect();
    commands.remove_resource::<RenetClient>();
    if *client_state.current() != ClientState::Stopped {
        client_state.set(ClientState::Stopped).unwrap();
    }
}

fn send_message_to_server(mut client: ResMut<RenetClient>) {
    let channel_id = 0;
    client.send_message(channel_id, "hello, server!".as_bytes().to_vec());
}

fn send_message_to_client(mut server: ResMut<RenetServer>) {
    let channel_id = 0;
    server.broadcast_message(channel_id, "hello, client!".as_bytes().to_vec());
}

fn receive_message_as_client(mut client: ResMut<RenetClient>) {
    let channel_id = 0;
    while let Some(message) = client.receive_message(channel_id) {
        let message: String = String::from_utf8_lossy(message.as_slice()).into_owned();
        println!("Client got message: {}", message);
    }
}

fn receive_message_as_server(mut server: ResMut<RenetServer>) {
    let channel_id = 0;
    // Send a text message for all clients
    for client_id in server.clients_id().into_iter() {
        while let Some(message) = server.receive_message(client_id, channel_id) {
            let message: String = String::from_utf8_lossy(message.as_slice()).into_owned();
            println!("Server got message: {}", message);
        }
    }
}

fn handle_events_as_server(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<CustomMaterial>>,
    mut server_events: EventReader<ServerEvent>)
{
    for event in server_events.iter() {
        match event {
            ServerEvent::ClientConnected(id, _) => {
                println!("Client {} connected", id);

                // spawn player on the server
                commands.spawn().insert_bundle(MaterialMeshBundle {
                    mesh: meshes.add(Mesh::from(shape::Torus { radius: 2.0, ring_radius: 1.0, subdivisions_segments: 24, subdivisions_sides: 64 })),
                    material: materials.add(CustomMaterial{
                        color: Color::rgb(1.0, 0.0, 0.0),
                        enabled: true,
                        ..default()
                    }),
                    transform: Transform::from_xyz(0.0, 0.5, 0.0),
                    ..default()
                }).insert(Player {});

                // send message to player to create itself
            }

            ServerEvent::ClientDisconnected
            (id) => {
                println!("Client {} disconnected", id);
            }
        }
    }
}

fn enter_multiplayer(
    mut commands: Commands, input: Res<Input<KeyCode>>, mut server_state: ResMut<State<ServerState>>, mut client_state: ResMut<State<ClientState>>) {
    if input.just_pressed(KeyCode::F4) {
        println!("Entering HOST mode");
        if *client_state.current() != ClientState::Stopped && *client_state.current() != ClientState::Stopping {
            client_state.set(ClientState::Stopping).unwrap();
        }
        if *server_state.current() != ServerState::Running {
            server_state.set(ServerState::Running).unwrap();
        }
        commands.insert_resource(MultiplayerKind::Host);
    }

    if input.just_pressed(KeyCode::F5) {
        println!("Entering LOCAL mode");
        if *client_state.current() != ClientState::Stopped && *client_state.current() != ClientState::Stopping {
            client_state.set(ClientState::Stopping).unwrap();
        }
        if *server_state.current() != ServerState::Stopped && *server_state.current() != ServerState::Stopping {
            server_state.set(ServerState::Stopping).unwrap();
        }
        commands.insert_resource(MultiplayerKind::Local);
    }

    if input.just_pressed(KeyCode::F6) {
        println!("Entering CLIENT mode");
        if *server_state.current() != ServerState::Stopped && *server_state.current() != ServerState::Stopping {
            server_state.set(ServerState::Stopping).unwrap();
        }
        if *client_state.current() != ClientState::Running {
            client_state.set(ClientState::Running).unwrap();
        }
        commands.insert_resource(MultiplayerKind::Client);
    }
}

fn move_torus(time: Res<Time>, input: Res<Input<KeyCode>>, mut query: Query<(&Player, &mut Transform)>) {
    for (_, mut transform) in query.iter_mut() {
        if input.pressed(KeyCode::W) {
            transform.translation.z += (3.0 * time.delta_seconds());
        }

        if input.pressed(KeyCode::S) {
            transform.translation.z -= (3.0 * time.delta_seconds());
        }

        if input.pressed(KeyCode::A) {
            transform.translation.x -= (3.0 * time.delta_seconds());
        }

        if input.pressed(KeyCode::D) {
            transform.translation.x += (3.0 * time.delta_seconds());
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