use nom::{
    bytes::streaming::tag,
    character::streaming::alphanumeric1 as alphanumeric,
    combinator::{map, map_res},
    error::ParseError,
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

/// The opening tag of an XML node, e.g. <name k="v">
#[derive(Clone, Eq, PartialEq, Debug)]
struct OpenNode<'a> {
    name: &'a str,
    attrs: Vec<(&'a str, &'a str)>,
}

impl<'a> OpenNode<'a> {
    #[cfg(test)]
    fn new(name: &'a str) -> Self {
        OpenNode {
            name,
            attrs: Vec::new(),
        }
    }
}

/// A whole XML node, e.g. <name k="v">...</name>
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Node<'a> {
    name: &'a str,
    attrs: Vec<(&'a str, &'a str)>,
    children: Vec<Node<'a>>,
}

impl<'a> Node<'a> {
    pub fn new(name: &'a str) -> Self {
        Node {
            name,
            attrs: Vec::new(),
            children: Vec::new(),
        }
    }
}

/// Parses </name> into "name"
fn parse_close_node<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &str, E> {
    // Uses three sub-parsers to match the literal </, then a series of alphanumeric characters,
    // then the literal >
    // The `delimited` call discards match 1 and match 3, keeping only match 2
    let parser = delimited(tag("</"), alphanumeric, tag(">"));
    parser(input)
}

/// Parse an XML attribute e.g. k="v"
fn parse_attribute<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (&str, &str), E> {
    // Match all the parts of an attribute
    // `tuple` lets us use any number of sub-parsers and calls them in order.
    let parser = tuple((tag(" "), alphanumeric, tag("=\""), alphanumeric, tag("\"")));
    // Take in all 5 matches and only keep the 2 we're interested in, i.e. the key and value.
    let kv = map(parser, |(_, k, _, v, _)| (k, v));
    kv(input)
}

fn parse_open_node<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, OpenNode, E> {
    let parser = tuple((tag("<"), alphanumeric, many0(parse_attribute), tag(">")));
    map(parser, |(_, name, attrs, _)| OpenNode { name, attrs })(input)
}

pub fn parse_node<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, E> {
    let parser = tuple((parse_open_node, many0(parse_node), parse_close_node));
    let node_parser = map_res(parser, |(open, children, close)| {
        if open.name == close {
            Ok(Node {
                name: open.name,
                children,
                attrs: open.attrs,
            })
        } else {
            Err(format!("{} != {}", open.name, close))
        }
    });
    node_parser(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::ErrorKind;

    #[test]
    fn open_node() {
        assert_eq!(
            parse_open_node::<(&str, ErrorKind)>("<note>"),
            Ok(("", OpenNode::new("note")))
        );
    }
    #[test]
    fn attribute() {
        assert_eq!(
            parse_attribute::<(&str, ErrorKind)>(" key=\"value\""),
            Ok(("", ("key", "value")))
        );
    }

    #[test]
    fn close_node() {
        assert_eq!(
            parse_close_node::<(&str, ErrorKind)>("</note>"),
            Ok(("", "note"))
        );
    }

    #[test]
    fn leaf() {
        assert_eq!(
            parse_node::<(&str, ErrorKind)>("<note k=\"v\" k2=\"v2\"></note>"),
            Ok((
                "",
                Node {
                    name: "note",
                    attrs: vec![("k", "v"), ("k2", "v2")],
                    children: Vec::new(),
                }
            ))
        );
    }

    #[test]
    fn tree() {
        assert_eq!(
            parse_node::<(&str, ErrorKind)>("<tree><leaf0></leaf0></tree>"),
            Ok((
                "",
                Node {
                    name: "tree",
                    children: vec![Node::new("leaf0")],
                    attrs: vec![]
                }
            ))
        );
    }
}
