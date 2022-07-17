use std::f32::consts::PI;

use bevy::app::App;
use bevy::DefaultPlugins;
use bevy::ecs::system::lifetimeless::SRes;
use bevy::ecs::system::SystemParamItem;
use bevy::pbr::{MaterialMeshBundle, MaterialPipeline, PbrBundle, PointLightBundle, SpecializedMaterial};
use bevy::prelude::{Assets, Camera, Image, MaterialPlugin, Vec2, Vec4};
use bevy::prelude::AssetServer;
use bevy::prelude::Color;
use bevy::prelude::Commands;
use bevy::prelude::Handle;
use bevy::prelude::Input;
use bevy::prelude::KeyCode;
use bevy::prelude::Mesh;
use bevy::prelude::Msaa;
use bevy::prelude::PerspectiveCameraBundle;
use bevy::prelude::PointLight;
use bevy::prelude::Quat;
use bevy::prelude::Query;
use bevy::prelude::Res;
use bevy::prelude::ResMut;
use bevy::prelude::Shader;
use bevy::prelude::shape;
use bevy::prelude::StandardMaterial;
use bevy::prelude::Time;
use bevy::prelude::Transform;
use bevy::prelude::Vec3;
use bevy::prelude::With;
use bevy::reflect::TypeUuid;
use bevy::render::mesh::MeshVertexBufferLayout;
use bevy::render::render_asset::{PrepareAssetError, RenderAsset, RenderAssets};
use bevy::render::render_resource::{BindGroup, BindGroupDescriptor, BindGroupEntry, BindGroupLayout, BindGroupLayoutDescriptor, BindGroupLayoutEntry, BindingResource, BindingType, Buffer, BufferBindingType, BufferInitDescriptor, BufferSize, BufferUsages, RenderPipelineDescriptor, SamplerBindingType, ShaderStages, SpecializedMeshPipelineError, TextureSampleType, TextureViewDimension};
use bevy::render::render_resource::std140::{AsStd140, Std140, WriteStd140};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::{RenderApp, RenderStage};
use bevy::utils::default;
use bevy_config_cam::ConfigCam;
use rand::Rng;

fn main() {
    let mut app = App::new();

    // resources
    app.insert_resource(Msaa { samples: 4 })
        .add_plugins(DefaultPlugins)
        .add_plugin(MaterialPlugin::<CustomMaterial>::default())
        .add_plugin(ConfigCam)
        .add_startup_system(setup)
        .add_system(disco)
        .add_system(orbit);

    // render stages
    app.sub_app_mut(RenderApp)
        .add_system_to_stage(RenderStage::Extract, extract_camera_eye)
        .add_system_to_stage(RenderStage::Prepare, prepare_camera_eye);

    app.run();
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials1: ResMut<Assets<StandardMaterial>>,
    mut materials2: ResMut<Assets<CustomMaterial>>,
    asset_server: Res<AssetServer>,
) {
    // ground
    commands.spawn_bundle(MaterialMeshBundle {
        mesh: meshes.add(Mesh::from(shape::Plane { size: 3000.0 })),
        material: materials2.add(CustomMaterial {
            color: Color::rgb(0.7, 0.7, 0.7),
            ..default()
        }),
        transform: Transform::from_xyz(0.0, 0.0, 0.0),
        ..default()
    });

    // cube
    commands.spawn().insert_bundle(MaterialMeshBundle {
        mesh: meshes.add(Mesh::from(shape::Cube { size: 1.0 })),
        material: materials2.add(CustomMaterial{
            color: Color::rgb(1.0, 0.0, 0.0),
            ..default()
        }),
        transform: Transform::from_xyz(0.0, 0.5, 0.0),
        ..default()
    });

    /*
    commands.spawn_bundle(PerspectiveCameraBundle {
        transform: Transform::from_xyz(-2.0, 2.5, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
    */

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

fn orbit(time: Res<Time>, mut query: Query<&mut Transform, With<PointLight>>) {
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

fn extract_camera_eye(mut commands: Commands, query: Query<&Transform, With<Camera>>) {
    for camera in query.iter() {
        commands.insert_resource(ExtractedCameraEye {
            camera_origin: camera.translation
        });
    }
}

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

struct ExtractedCameraEye {
    camera_origin: Vec3
}


// This is the struct that will be passed to your shader
#[derive(Debug, Clone, TypeUuid)]
#[uuid = "4ee9c363-1124-4113-890e-199d81b00281"]
pub struct CustomMaterial {
    color: Color,
    origin: Vec3
    // pos0_texture: Handle<Image>,
    // pos1_texture: Handle<Image>,
    // smoke_mask_texture: Handle<Image>,
}

#[derive(Debug, Clone, AsStd140)]
struct CustomMaterialUniformData {
    color: Vec4,
    origin: Vec3
}

impl Default for CustomMaterial {
    fn default() -> CustomMaterial {
        CustomMaterial {
            color: Color::rgba(1.0, 1.0, 1.0, 1.0),
            origin: Vec3::new(0.0, 0.0, 0.0)
        }
    }
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
        SRes<RenderAssets<Image>>,
        SRes<MaterialPipeline<Self>>
    );
    fn extract_asset(&self) -> Self::ExtractedAsset {
        self.clone()
    }

    fn prepare_asset(
        extracted_asset: Self::ExtractedAsset,
        (render_device, gpu_images, material_pipeline): &mut SystemParamItem<Self::Param>,
    ) -> Result<Self::PreparedAsset, PrepareAssetError<Self::ExtractedAsset>> {
        // prepare attributes passed to shader
        let uniform_data = CustomMaterialUniformData {
            color: extracted_asset.color.as_linear_rgba_f32().into(),
            origin: extracted_asset.origin,
        };

        // let pi = Vec2::new(PI, PI/180.0);
        // let gamma = Vec2::new(2.2, 1.0/2.2);
        // let sun_position = Vec2::new(260.0, 0.0);
        // let origin = Vec2::new(0.0, 0.0);
        // let near_far = Vec2::new(10.0, 200.0);
        // let enabled = Vec2::new(1f32, 1f32);
        // let background_color0 = Color::rgba(0.4, 0.8, 0.9, 1.0);
        // let background_color1 = Color::rgba(0.8, 0.2, 0.0, 1.0);

        // let base_image = match gpu_images.get(&extracted_asset.pos0_texture) {
        //     Some(base_image) => base_image,
        //     None => return Err(PrepareAssetError::RetryNextUpdate(extracted_asset))
        // };
        //
        // let fog_image = match gpu_images.get(&extracted_asset.pos1_texture) {
        //     Some(fog_image) => fog_image,
        //     None => return Err(PrepareAssetError::RetryNextUpdate(extracted_asset))
        // };
        //
        // let bloom_image = match gpu_images.get(&extracted_asset.smoke_mask_texture) {
        //     Some(bloom_image) => bloom_image,
        //     None => return Err(PrepareAssetError::RetryNextUpdate(extracted_asset))
        // };

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

                // BindGroupEntry {
                //     binding: 1,
                //     resource: BindingResource::TextureView(&base_image.texture_view),
                // },
                //
                // BindGroupEntry {
                //     binding: 2,
                //     resource: BindingResource::Sampler(&base_image.sampler),
                // },
                //
                // BindGroupEntry {
                //     binding: 3,
                //     resource: BindingResource::TextureView(&fog_image.texture_view),
                // },
                //
                // BindGroupEntry {
                //     binding: 4,
                //     resource: BindingResource::Sampler(&fog_image.sampler),
                // },
                //
                // BindGroupEntry {
                //     binding: 5,
                //     resource: BindingResource::TextureView(&bloom_image.texture_view),
                // },
                //
                // BindGroupEntry {
                //     binding: 6,
                //     resource: BindingResource::Sampler(&bloom_image.sampler),
                // },
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
        Some(asset_server.load("shaders/custom_material.frag"))
    }

    fn bind_group(render_asset: &<Self as RenderAsset>::PreparedAsset) -> &BindGroup {
        &render_asset.bind_group
    }

    fn bind_group_layout(render_device: &RenderDevice) -> BindGroupLayout {
        render_device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            entries: &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: BufferSize::new(CustomMaterialUniformData::std140_size_static() as u64),
                    },
                    count: None,
                },

                // BindGroupLayoutEntry {
                //     binding: 1,
                //     visibility: ShaderStages::FRAGMENT,
                //     ty: BindingType::Texture {
                //         sample_type: TextureSampleType::Float { filterable: true },
                //         view_dimension: TextureViewDimension::D2,
                //         multisampled: false,
                //     },
                //     count: None,
                // },

                // BindGroupLayoutEntry {
                //     binding: 2,
                //     visibility: ShaderStages::FRAGMENT,
                //     ty: BindingType::Sampler(SamplerBindingType::Filtering),
                //     count: None,
                // },
                //
                // BindGroupLayoutEntry {
                //     binding: 3,
                //     visibility: ShaderStages::FRAGMENT,
                //     ty: BindingType::Texture {
                //         sample_type: TextureSampleType::Float { filterable: true },
                //         view_dimension: TextureViewDimension::D2,
                //         multisampled: false,
                //     },
                //     count: None,
                // },
                //
                // BindGroupLayoutEntry {
                //     binding: 4,
                //     visibility: ShaderStages::FRAGMENT,
                //     ty: BindingType::Sampler(SamplerBindingType::Filtering),
                //     count: None,
                // },
                //
                // BindGroupLayoutEntry {
                //     binding: 5,
                //     visibility: ShaderStages::FRAGMENT,
                //     ty: BindingType::Texture {
                //         sample_type: TextureSampleType::Float { filterable: true },
                //         view_dimension: TextureViewDimension::D2,
                //         multisampled: false,
                //     },
                //     count: None,
                // },
                //
                // BindGroupLayoutEntry {
                //     binding: 6,
                //     visibility: ShaderStages::FRAGMENT,
                //     ty: BindingType::Sampler(SamplerBindingType::Filtering),
                //     count: None,
                // },
            ],
            label: None,
        })
    }
}
