const std = @import("std");

const Parser = @import("../../osc.zig").Parser;
const Command = @import("../../osc.zig").Command;

/// Parse OSC 50: Set or query the terminal font (XTerm compatibility).
/// When value is "?" this is a query for the current font.
pub fn parse(parser: *Parser, terminator_ch: ?u8) ?*Command {
    const cap = if (parser.capture) |*c| c else {
        parser.state = .invalid;
        return null;
    };
    cap.writer.writeByte(0) catch {
        parser.state = .invalid;
        return null;
    };
    const data = cap.trailing();
    parser.command = .{
        .set_font = .{
            .value = data[0 .. data.len - 1 :0],
            .terminator = .init(terminator_ch),
        },
    };
    return &parser.command;
}

test "OSC 50: set_font query" {
    const testing = std.testing;

    var p: Parser = .init(null);
    p.next('5');
    p.next('0');
    p.next(';');
    p.next('?');
    const cmd = p.end(null).?.*;
    try testing.expect(cmd == .set_font);
    try testing.expectEqualStrings("?", cmd.set_font.value);
}

test "OSC 50: set_font value" {
    const testing = std.testing;

    var p: Parser = .init(null);
    p.next('5');
    p.next('0');
    p.next(';');
    p.next('F');
    p.next('i');
    p.next('r');
    p.next('a');
    p.next(' ');
    p.next('C');
    p.next('o');
    p.next('d');
    p.next('e');
    const cmd = p.end(null).?.*;
    try testing.expect(cmd == .set_font);
    try testing.expectEqualStrings("Fira Code", cmd.set_font.value);
}

test "OSC 50: set_font with font size" {
    const testing = std.testing;

    var p: Parser = .init(null);
    p.next('5');
    p.next('0');
    p.next(';');
    p.next('F');
    p.next('i');
    p.next('r');
    p.next('a');
    p.next('C');
    p.next('o');
    p.next('d');
    p.next('e');
    p.next(':');
    p.next('s');
    p.next('i');
    p.next('z');
    p.next('e');
    p.next('=');
    p.next('1');
    p.next('4');
    const cmd = p.end(null).?.*;
    try testing.expect(cmd == .set_font);
    try testing.expectEqualStrings("FiraCode:size=14", cmd.set_font.value);
}

test "OSC 50: set_font empty" {
    const testing = std.testing;

    var p: Parser = .init(null);
    p.next('5');
    p.next('0');
    p.next(';');
    const cmd = p.end(null).?.*;
    try testing.expect(cmd == .set_font);
    try testing.expectEqualStrings("", cmd.set_font.value);
}
