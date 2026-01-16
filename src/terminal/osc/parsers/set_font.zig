const std = @import("std");
const Parser = @import("../../osc.zig").Parser;
const Command = @import("../../osc.zig").Command;

/// Parse OSC 50 (set or query font)
pub fn parse(parser: *Parser, terminator_ch: ?u8) ?*Command {
    const writer = parser.writer orelse {
        parser.state = .invalid;
        return null;
    };
    writer.writeByte(0) catch {
        parser.state = .invalid;
        return null;
    };
    const data = writer.buffered();
    parser.command = .{
        .set_font = .{
            .value = data[0 .. data.len - 1 :0],
            .terminator = .init(terminator_ch),
        },
    };
    return &parser.command;
}

test "OSC 50: set font" {
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

test "OSC 50: set font with value" {
    const testing = std.testing;

    var p: Parser = .init(null);
    p.next('5');
    p.next('0');
    p.next(';');
    for ("Menlo:size=12") |c| p.next(c);
    const cmd = p.end(null).?.*;
    try testing.expect(cmd == .set_font);
    try testing.expectEqualStrings("Menlo:size=12", cmd.set_font.value);
}
