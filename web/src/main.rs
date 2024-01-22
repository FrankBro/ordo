use ordo::env::{Env, Error, Output};
use web_sys::HtmlInputElement;
use yew::prelude::*;

const ENTER_KEY: u32 = 13;

enum Message {
    ProcessLine(String),
}

#[derive(Default)]
struct App {
    env: Env,
    history: Vec<(String, Result<Output, Error>)>,
}

impl Component for App {
    type Message = Message;

    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self::default()
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Message::ProcessLine(source) => {
                let res = self.env.process(&source);
                self.history.push((source, res));
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link().clone();
        let onkeydown = move |e: KeyboardEvent| {
            if e.key_code() == ENTER_KEY {
                let input: HtmlInputElement = e.target_unchecked_into();
                let line = input.value();
                input.set_value("");
                link.send_message(Message::ProcessLine(line))
            }
        };
        html! {
            <div style="display: flex; flex-direction: row; height: 90vh;">
                <div style="flex: 2; display: flex; flex-direction: column">
                    <fieldset style="flex: 9; overflow-y: scroll;">
                        { render_legend("History") }
                        { self.render_history() }
                    </fieldset>
                    <fieldset style="flex: 1;">
                        { render_legend("Input") }
                        <input type="text" placeholder="Type here" {onkeydown} style="border: 0; outline: 0; width: 100%; height: 100%; font-size: 2em;"/>
                    </fieldset>
                </div>
                <fieldset style="flex: 1; overflow-y: scroll;">
                    { render_legend("Environment") }
                    { self.render_env() }
                </fieldset>
            </div>
        }
    }
}

impl App {
    fn render_history(&self) -> Html {
        let history = self
            .history
            .iter()
            .map(|(line, res)| render_line(line, res));
        html! {
            <div>
                { for history }
            </div>
        }
    }

    fn render_env(&self) -> Html {
        let env = self
            .env
            .infer
            .vars
            .iter()
            .filter_map(|(var, ty)| {
                self.env.eval.vars.get(var).map(|val| {
                    (
                        var,
                        self.env.infer.ty_to_string(ty).unwrap(),
                        val.to_string(),
                    )
                })
            })
            .map(|(var, ty, val)| render_input(var, &ty, &val));
        html! {
            <div>
                { for env }
            </div>
        }
    }
}

fn render_input(input: &String, ty: &String, val: &String) -> Html {
    html! {
        <div style="display: flex; flex-direction: column;">
            <div style="flex: 2; font-size: 1.5rem;">{input}</div>
            <div style="flex: 1;">
                <div style="float: left">{val}</div>
                <div style="float: right">{ty}</div>
            </div>
        </div>
    }
}

fn render_legend(title: &str) -> Html {
    html! {
        <legend><h2>{title}</h2></legend>
    }
}

fn render_line(line: &String, res: &Result<Output, Error>) -> Html {
    match res {
        Ok(output) => render_input(line, &output.ty, &output.val),
        Err(e) => {
            let value = e.to_string();
            html! {
                <div style="display: flex; flex-direction: column;">
                    <div style="flex: 2; font-size: 1.5rem;">{line}</div>
                    <div style="flex: 1;">{value}</div>
                </div>
            }
        }
    }
}

fn main() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Trace));
    yew::Renderer::<App>::new().render();
}
