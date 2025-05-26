# small-array-list

This library provides a `SmallArrayList` type (and some variants), which is
optimized for memory efficient storage of arrays that generally contain few
items. This works by sharing the memory internal storage space between either
a fixed size array, or an externally allocated slice, and switching between
the two as needed. See the [Small Capacity Limit](#small-capacity-limit) for
details on this storage mechanism.

```zig
// We'll just use SmallArrayList for this example, but there are variants
// that allow further parameterization.
const SmallArrayList = @import("small_array_list").SmallArrayList;

// Setup whichever allocator you'd like to use.
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
defer arena.deinit();
const allocator = arena.allocator();

var list: SmallArrayList(i32) = .empty;
defer list.deinit(allocator);

// Append works just like the std.array_list.ArrayListUnmanaged append.
try list.append(allocator, 1);

// Nearly all of the standard library methods are available.
var many = try list.addManyAsArray(allocator, 2);
many[0] = 2;
many[1] = 3;

// Unlike ArrayList, items cannot be accessed directly as a slice.
for (list.items()) |item| {
    std.debug.print("item={}\n", .{item});
}

// You can see if the SmallArrayList is using an allocation or not.
if (!list.hasAllocation()) {
    std.debug.print("no allocations!\n", .{});
}

// Unlike ArrayList, len is a top-level field.
// On 64-bit systems, this will be: len=3 capacity=4
std.debug.print("len={} capacity={}\n", .{ list.len, list.capacity });
```

> [!IMPORTANT]
> Instances keep much of the interface in common with `std.array_list.ArrayListUnmanaged`
> with a few important differences:
>
> 1. To get the number of items in the list, use `list.len` instead of `list.items.len`.
> 1. To get the items of in the list, use `list.items()` instead of `list.items`.

## Installing

First, add the dependency to your `build.zig.zon`:

```sh
zig fetch --save git+https://github.com/kalamay/small-array-list#v0.1.1
```

Next add the dependecy to your `build.zig`:

```zig
const small_array_list_mod = b.dependency("small_array_list", .{
    .target = target,
    .optimize = optimize,
}).module("small_array_list");

exe_mod.addImport("small_array_list", small_array_list_mod);
// or lib_mod, or any other module
```

Now you can import the library:

```zig
const small_array_list = @import("small_array_list");
```

### Using a Different Name

If you'd like to use a different name within your project, you can choose a
different import name for the module:

```zig
const small_array_list_mod = b.dependency("small_array_list", .{
    .target = target,
    .optimize = optimize,
}).module("small_array_list");

exe_mod.addImport("array", small_array_list_mod);
```

And then you'd be able to import it as `"array"`:

```zig
const array = @import("array");
```

## Small Capacity Limit

The available small capacity limit is determined by the type stored and the
native machine word size. Because a zig slice requires two machine words for
storing it's `ptr` and `len`, this space can be used for direct item storage
instead until the small capacity is exceeded. For any given type `T`, the
small array list can store up to `2*@sizeOf(usize) / @sizeOf(T)` items before
requiring any internal allocation. 

For example on a 64-bit processor will print out `smallCapacity=4 sizeOf=24`:

```zig
const List = SmallArrayList(i32);
std.debug.print("smallCapacity={} sizeOf={}\n", .{List.smallCapacity, @sizeOf(List)});
```

The over size of a `SmallArrayList` is generally three machine words. This is
achieved using a few trade-offs:

1. The overlapped memory of the internal array and external slice is achieved
using a union. By ensuring this union is indiscriminate, the array list can
maximize storage efficiency. This union is then discriminated using the
`capacity` field of the array list. Using `SmallArrayListSized`, you can set
a small capacity limit that exceeds the default size. This will cause the
overall small array list size to grow.

2. The `len` and `capacity` are each half of a `usize`. This does mean it will result
in an `error.OutOfMemory` when trying to allocate a capacity greater than
half the maximum of a `std.array_list.ArrayList`. However, this library is
optimized for small array lists. If lists of such size are needed, the standard
library should be used.

3. All `SmallArrayList` types are unmanaged, meaning they do not store the
`std.mem.Allocator` internally, and each function that could possibly allocate
or deallocate takes the allocator as a parameter. Note that the same allocator
instance must be used for each call into any single small array list.

> [!IMPORTANT]
> One important thing to keep in mind when using larger types is that there is
> a minimum small capacity of 1, so if the size of `T` exceeds two machine words,
> the overall size of the `SmallArrayList` will expand. This can still be
> beneficial, but it is something you'll want to consider.

## Variants

The `SmallArrayList` uses the default alignment of `T` and a small capacity
determined by how many items of `T` can be stored in two machine words. However,
both of these values may be overridden.

If you want to change the alignment, you can use either `SmallArrayListAligned`
or `SmallArrayListAlignedSized`, passing in the desired alignment.

Changing the small capacity can be done using either `SmallArrayListSized` or
`SmallArrayListAlignedSized`. Using a small capacity larger than the default
will increase the overall size of the `SmallArrayList`, but it allows for
storing more items before allocating.

For example, on a 64-bit system:

```zig
const std = @import("std");
const expect = std.testing.expect;

test "sizes" {
    const a = testing.allocator;

    const List1 = SmallArrayList(i32);
    const List2 = SmallArrayListSized(i32, 6);

    // This holds true on a 64-bit system
    try testing.expect(@sizeOf(List1) == 24);
    try testing.expect(@sizeOf(List2) == 32);

    var list1: List1 = .empty;
    var list2: List2 = .empty;

    defer list1.deinit(a);
    defer list2.deinit(a); // don't strictly need to deinit list2

    for (0..List2.smallCapacity) |i| {
        try list1.append(a, @intCast(i));
        try list2.append(a, @intCast(i));
    }

    try testing.expect(list1.hasAllocation());
    try testing.expect(!list2.hasAllocation());
}
```
