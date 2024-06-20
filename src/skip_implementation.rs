#[derive(Debug, PartialEq, Default)]
pub struct Skip {
    pub from_impl: bool,
    pub try_into_impl: bool,
}

impl Skip {
    pub fn parse_segment(&mut self, ident: &syn::Ident) {
        match ident.to_string().as_str() {
            "skip_from" => self.from_impl = true,
            "skip_try_into" => self.try_into_impl = true,
            _ => {}
        }
    }
}
