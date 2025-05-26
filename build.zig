const std = @import("std");

pub fn build(b: *std.Build) void {
    const root_source_file = "src/root.zig";

    const unit_tests = b.addTest(.{
        .root_source_file = b.path(root_source_file),
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    _ = b.addModule("small_array_list", .{
        .root_source_file = b.path(root_source_file),
    });
}
