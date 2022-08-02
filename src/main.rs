use bevy::DefaultPlugins;
use bevy::prelude::{App, AssetServer, FromWorld, Handle, Image, Local, ResMut, World};
use bevy_egui::{egui, EguiContext, EguiPlugin};
use crate::egui::Vec2;

struct Images {
    button_1: Handle<Image>,
}

impl FromWorld for Images {
    fn from_world(world: &mut World) -> Self {
        let asset_server = world.get_resource_mut::<AssetServer>().unwrap();
        Self {
            button_1: asset_server.load("textures/Big_Rectangle.png"),
        }
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(EguiPlugin)
        .add_system(ui_example)
        .run();
}

fn ui_example(
    mut egui_context: ResMut<EguiContext>,
    mut images: Local<Images>,
) {
    let img = egui_context.add_image(images.button_1.clone());
    let img_size = Vec2::new(300., 20.);

    egui::Window::new("Hello").show(egui_context.ctx_mut(), |ui| {
        ui.add(egui::ImageButton::new(img, img_size))
    });
}

