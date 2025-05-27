// SPDXLicenseIdentifier: MIT
// copyrightText: Copyright (c) Zig contributors
// copyrightText: Copyright (c) Jeremy Larkin
// attributionText: This package was developed by Jeremy Larkin using source ported from the zig standard library.

const std = @import("std");
const mem = std.mem;
const math = std.math;
const assert = std.debug.assert;
const testing = std.testing;
const Allocator = mem.Allocator;
const ceilPowerOfTwo = math.ceilPowerOfTwo;

/// Type used for the SmallArrayList `len` and `capacity` fields. This is half
/// of the native word size.
pub const Size = std.meta.Int(.unsigned, @bitSizeOf(usize) / 2);

/// Calculates the small capacity to be used by SmallArrayList for type `T`.
/// This determines how many of `T` can fit in two machine word sizes, as this
/// is the largest capacity that can be supported without increasing the size
/// of the SmallArrayList. However, a minimum of 1 is returned. If `T` is larger
/// than two machine words, this size will increase the of SmallArrayList's
/// defined using this capacity.
pub fn defaultSmallCapacity(comptime T: type) comptime_int {
    return @max(1, @sizeOf([]T) / @sizeOf(T));
}

/// A contiguous, growable list of items in memory with a small capacity to
/// hold items internally before requiring any allocations.
/// `SmallArrayList` defines a `SmallArrayListAlignedSized` type with the small
/// capacity determined by `defaultSmallCapacity(T)` and the T values are
/// aligned using their default alignment.
///
/// Functions that potentially allocate memory accept an `Allocator` parameter.
/// Initialize directly using `.empty` or with `initCapacity`, and deinitialize
/// with `deinit`.
pub fn SmallArrayList(comptime T: type) type {
    return SmallArrayListAlignedSized(T, null, defaultSmallCapacity(T));
}

/// A contiguous, growable list of arbitrarily aligned items in memory with
/// a small capacity to hold items internally before requiring any allocations.
/// `SmallArrayListAligned` defines a `SmallArrayListAlignedSized` type with the
/// small capacity determined by `defaultSmallCapacity(T)` and the T values are
/// aligned using `alignment`.
///
/// Functions that potentially allocate memory accept an `Allocator` parameter.
/// Initialize directly using `.empty` or with `initCapacity`, and deinitialize
/// with `deinit`.
pub fn SmallArrayListAligned(comptime T: type, comptime alignment: u29) type {
    return SmallArrayListAlignedSized(T, alignment, defaultSmallCapacity(T));
}

/// A contiguous, growable list of items in memory with a small capacity to
/// hold items internally before requiring any allocations.
/// `SmallArrayListSized` defines a `SmallArrayListAlignedSized` type with the
/// small set to `size` and the T values are aligned using their default alignment.
///
/// Functions that potentially allocate memory accept an `Allocator` parameter.
/// Initialize directly using `.empty` or with `initCapacity`, and deinitialize
/// with `deinit`.
pub fn SmallArrayListSized(comptime T: type, comptime size: usize) type {
    return SmallArrayListAlignedSized(T, null, size);
}

/// A contiguous, growable list of arbitrarily aligned items in memory with
/// a small capacity to hold items internally before requiring any allocations.
/// This is a wrapper around a seqeuntail storage of T values that switches
/// from an array to a slice as the length exceeds a predetermined capacity.
/// This small capacity is set to `size`. The T values are aligned to `alignment`-byte
/// addresses. If the specified alignment is `null`, then `@alignOf(T)` is used.
///
/// Functions that potentially allocate memory accept an `Allocator` parameter.
/// Initialize directly using `.empty` or with `initCapacity`, and deinitialize
/// with `deinit`.
pub fn SmallArrayListAlignedSized(comptime T: type, comptime alignment: ?u29, comptime size: Size) type {
    if (alignment) |a| {
        if (a == @alignOf(T)) {
            return SmallArrayListAlignedSized(T, size, null);
        }
    }

    const SliceType = if (alignment) |a| ([]align(a) T) else []T;
    const SliceConstType = if (alignment) |a| ([]align(a) const T) else []const T;

    const arraySize = @max(defaultSmallCapacity(T), size);

    const Union = unchecked: {
        @setRuntimeSafety(false);
        break :unchecked if (alignment) |a| union {
            array: [arraySize]T align(a),
            slice: SliceType,
        } else union {
            array: [arraySize]T,
            slice: SliceType,
        };
    };

    return struct {
        const Self = @This();

        // The maximum item count that may be stored in the SmallArrayList
        // without requiring any allocations.
        pub const smallCapacity: Size = arraySize;

        // Unchecked union of either an array or slice. The union uses the len field as
        // the discriminator. When `len` <= `size`, the `array` field is active,
        // otherwise the `slice` field is. This field is not intended to be accessed
        // directly. Use the `items()` method instead.
        as: Union = .{ .array = undefined },
        /// How many T values are defined in the list. Do not modify the value.
        len: Size = 0,
        /// How many T values this list can hold without allocating additional memory.
        /// Do not modify the value.
        capacity: Size = smallCapacity,

        /// An SmallArrayList containing no elements.
        pub const empty: Self = .{};

        pub const Slice = SliceType;
        pub const SliceConst = SliceConstType;

        /// Initialize with capacity to hold `num` elements.
        /// The resulting capacity will equal `num` exactly.
        /// Deinitialize with `deinit`.
        pub fn initCapacity(gpa: Allocator, num: usize) Allocator.Error!Self {
            var self = Self.empty;
            try self.ensureTotalCapacityPrecise(gpa, num);
            return self;
        }

        /// Release all allocated memory.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (self.hasAllocation()) {
                gpa.free(self.as.slice);
            }
        }

        /// Creates a copy of this array list.
        pub fn clone(self: Self, gpa: Allocator) Allocator.Error!Self {
            var cloned = Self.empty;
            try cloned.appendSlice(gpa, self.itemsConst());
            return cloned;
        }

        /// Test if the small array list has an external allocation. Calls to
        /// `deinit` may be omitted if this is false.
        pub inline fn hasAllocation(self: *const Self) bool {
            return self.capacity > smallCapacity;
        }

        /// Gets the items of the SmallArrayList.
        pub inline fn items(self: *Self) Slice {
            return self.entireSlice()[0..self.len];
        }

        /// Gets the const items of the SmallArrayList.
        pub inline fn itemsConst(self: *const Self) SliceConst {
            return self.entireSliceConst()[0..self.len];
        }

        /// Returns a slice of all the items plus the extra capacity, whose
        /// memory contents are `undefined`. This slice may refer to the small
        /// array or an allocated slice.
        pub inline fn entireSlice(self: *Self) Slice {
            return if (self.hasAllocation()) self.as.slice else &self.as.array;
        }

        /// Returns a const slice of all the items plus the extra capacity,
        /// whose memory contents are `undefined`. This slice may refer to the
        /// small array or an allocated slice.
        pub inline fn entireSliceConst(self: *const Self) SliceConst {
            return if (self.hasAllocation()) self.as.slice else &self.as.array;
        }

        /// Returns the allocated slice if one is available of all the items
        /// plus the extra capacity, whose memory contents are `undefined`.
        pub inline fn allocatedSlice(self: Self) ?Slice {
            return if (self.hasAllocation()) return self.as.slice else null;
        }

        /// Returns a slice of only the extra capacity after items.
        /// This can be useful for writing directly into an SmallArrayList.
        /// Note that such an operation must be followed up with a direct
        /// modification of `self.len`.
        pub inline fn unusedCapacitySlice(self: *Self) []T {
            return self.entireSlice()[self.len..];
        }

        /// Returns the element from the list at `index`.
        /// Asserts that the index is within the list.
        pub inline fn get(self: Self, index: usize) T {
            return self.itemsConst()[index];
        }

        /// Returns the element from the list at `index` or `null` if the index is invalid.
        pub inline fn getOrNull(self: Self, index: usize) ?T {
            const s = self.itemsConst();
            return if (index < s.len) s[index] else null;
        }

        /// Returns the last element from the list.
        /// Asserts that the list is not empty.
        pub inline fn getLast(self: Self) T {
            return self.get(self.len - 1);
        }

        /// Returns the last element from the list, or `null` if list is empty.
        pub inline fn getLastOrNull(self: Self) ?T {
            const n = self.len;
            if (n == 0) return null;
            return self.entireSliceConst()[n - 1];
        }

        /// Replaces the element in the list at `index` with `value`.
        /// Asserts that the index is within the list.
        pub inline fn set(self: *Self, index: usize, value: T) void {
            self.items()[index] = value;
        }

        /// Extends the list by 1 element. Allocates more memory as necessary.
        /// Invalidates element pointers if additional memory is needed.
        pub fn append(self: *Self, gpa: Allocator, item: T) Allocator.Error!void {
            const dst = try self.addOne(gpa);
            dst.* = item;
        }

        /// Append the slice of items to the list. Allocates more
        /// memory as necessary.
        /// Invalidates element pointers if additional memory is needed.
        pub fn appendSlice(self: *Self, gpa: Allocator, slice: []const T) Allocator.Error!void {
            try self.ensureUnusedCapacity(gpa, slice.len);
            self.appendSliceAssumeCapacity(slice);
        }

        /// Append the slice of items to the list.
        /// Never invalidates element pointers.
        /// Asserts that the list can hold the additional items.
        pub fn appendSliceAssumeCapacity(self: *Self, slice: []const T) void {
            const old_len = self.len;
            assert(old_len + slice.len <= self.capacity);
            const new_len = old_len + @as(Size, @truncate(slice.len));
            self.len = @truncate(new_len);
            @memcpy(self.entireSlice()[old_len..][0..slice.len], slice);
        }

        /// Append the slice of items to the list. Allocates more
        /// memory as necessary. Only call this function if a call to `appendSlice` instead would
        /// be a compile error.
        /// Invalidates element pointers if additional memory is needed.
        pub fn appendUnalignedSlice(self: *Self, gpa: Allocator, slice: []align(1) const T) Allocator.Error!void {
            try self.ensureUnusedCapacity(gpa, slice.len);
            self.appendUnalignedSliceAssumeCapacity(slice);
        }

        /// Append an unaligned slice of items to the list.
        /// Only call this function if a call to `appendSliceAssumeCapacity`
        /// instead would be a compile error.
        /// Asserts that the list can hold the additional items.
        pub fn appendUnalignedSliceAssumeCapacity(self: *Self, slice: []align(1) const T) void {
            const old_len = self.len;
            const new_len = old_len + slice.len;
            assert(new_len <= self.capacity);
            self.len = @truncate(new_len);
            @memcpy(self.entireSlice()[old_len..][0..slice.len], slice);
        }

        /// Append a value to the list `n` times.
        /// Allocates more memory as necessary.
        /// Invalidates element pointers if additional memory is needed.
        /// The function is inline so that a comptime-known `value` parameter will
        /// have a more optimal memset codegen in case it has a repeated byte pattern.
        pub fn appendNTimes(self: *Self, gpa: Allocator, value: T, n: usize) Allocator.Error!void {
            const old_len = self.len;
            const new_len = old_len + n;
            try self.resize(gpa, new_len);
            @memset(self.items()[old_len..new_len], value);
        }

        /// Append a value to the list `n` times.
        /// Never invalidates element pointers.
        /// The function is inline so that a comptime-known `value` parameter will
        /// have better memset codegen in case it has a repeated byte pattern.
        /// Asserts that the list can hold the additional items.
        pub fn appendNTimesAssumeCapacity(self: *Self, value: T, n: usize) void {
            const old_len = self.len;
            const new_len = old_len + n;
            assert(new_len <= self.capacity);
            @memset(self.entireSlice()[old_len..new_len], value);
            self.len = @truncate(new_len);
        }

        /// Insert `item` at index `i`. Moves `list[i .. list.len]` to higher indices to make room.
        /// If `i` is equal to the length of the list this operation is equivalent to append.
        /// This operation is O(N).
        /// Invalidates element pointers if additional memory is needed.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn insert(self: *Self, gpa: Allocator, index: usize, item: T) Allocator.Error!void {
            const dst = try self.addManyAt(gpa, index, 1);
            dst[0] = item;
        }

        /// Insert `item` at index `i`. Moves `list[i .. list.len]` to higher indices to make room.
        /// If in` is equal to the length of the list this operation is equivalent to append.
        /// This operation is O(N).
        /// Asserts that the list has capacity for one additional item.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn insertAssumeCapacity(self: *Self, i: usize, item: T) void {
            const old_len = self.len;
            assert(old_len < self.capacity);
            const new_len = old_len + 1;
            const slice = self.entireSlice();
            slice.copyBackwards(T, slice[i + 1 .. new_len], slice[i..old_len]);
            slice[i] = item;
            self.len = new_len;
        }

        /// Insert slice `items` at index `i` by moving `list[i .. list.len]` to make room.
        /// This operation is O(N).
        /// Invalidates pre-existing pointers to elements at and after `index`.
        /// Invalidates all pre-existing element pointers if capacity must be
        /// increased to accommodate the new elements.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn insertSlice(self: *Self, gpa: Allocator, index: usize, slice: []const T) Allocator.Error!void {
            const dst = try self.addManyAt(gpa, index, slice.len);
            @memcpy(dst, slice);
        }

        /// Grows or shrinks the list as necessary.
        /// Invalidates element pointers if additional capacity is allocated.
        /// Asserts that the range is in bounds.
        pub fn replaceRange(
            self: *Self,
            gpa: Allocator,
            start: usize,
            len: usize,
            new_items: []const T,
        ) Allocator.Error!void {
            const after_range = start + len;
            const range = self.items()[start..after_range];
            if (range.len < new_items.len) {
                const first = new_items[0..range.len];
                const rest = new_items[range.len..];
                @memcpy(range[0..first.len], first);
                try self.insertSlice(gpa, after_range, rest);
            } else {
                self.replaceRangeAssumeCapacity(start, len, new_items);
            }
        }

        /// Grows or shrinks the list as necessary.
        /// Never invalidates element pointers.
        /// Asserts the capacity is enough for additional items.
        pub fn replaceRangeAssumeCapacity(self: *Self, start: usize, len: usize, new_items: []const T) void {
            const after_range = start + len;
            const slice = self.items();
            const range = slice[start..after_range];

            if (range.len == new_items.len)
                @memcpy(range[0..new_items.len], new_items)
            else if (range.len < new_items.len) {
                const first = new_items[0..range.len];
                const rest = new_items[range.len..];
                @memcpy(range[0..first.len], first);
                const dst = self.addManyAtAssumeCapacity(after_range, rest.len);
                @memcpy(dst, rest);
            } else {
                const old_len = self.len;
                const extra = range.len - new_items.len;
                @memcpy(range[0..new_items.len], new_items);
                mem.copyForwards(T, slice[after_range - extra ..], slice[after_range..]);
                @memset(slice[old_len - extra ..], undefined);
                self.len = old_len - @as(Size, @truncate(extra));
            }
        }

        /// Remove and return the last element from the list, or return `null` if list is empty.
        /// Invalidates element pointers to the removed element, if any.
        pub fn pop(self: *Self) ?T {
            const old_len = self.len;
            if (old_len == 0) return null;
            const new_len = old_len - 1;
            const val = self.entireSlice()[new_len];
            self.len = new_len;
            return val;
        }

        /// Remove the element at index `i`, shift elements after index
        /// `i` forward, and return the removed element.
        /// Invalidates element pointers to end of list.
        /// This operation is O(N).
        /// This preserves item order. Use `swapRemove` if order preservation is not important.
        /// Asserts that the index is in bounds.
        /// Asserts that the list is not empty.
        pub fn orderedRemove(self: *Self, i: usize) T {
            const old_item = self.items()[i];
            self.replaceRangeAssumeCapacity(i, 1, &.{});
            return old_item;
        }

        /// Removes the element at the specified index and returns it.
        /// The empty slot is filled from the end of the list.
        /// This operation is O(1).
        /// This may not preserve item order. Use `orderedRemove` if you need to preserve order.
        /// Asserts that the list is not empty.
        /// Asserts that the index is in bounds.
        pub fn swapRemove(self: *Self, i: usize) T {
            const slice = self.items();
            const old_item = slice[i];
            const new_len = self.len - 1;
            if (new_len != i) {
                slice[i] = slice[new_len];
            }
            self.len = new_len;
            return old_item;
        }

        /// Increase length by 1, returning pointer to the new item.
        /// The returned pointer becomes invalid when the list resized.
        pub fn addOne(self: *Self, gpa: Allocator) Allocator.Error!*T {
            const dst = try self.addManyAsArray(gpa, 1);
            return &dst[0];
        }

        /// Add `count` new elements at position `index`, which have
        /// `undefined` values. Returns a slice pointing to the newly allocated
        /// elements, which becomes invalid after various `SmallArrayList`
        /// operations.
        /// Invalidates pre-existing pointers to elements at and after `index`.
        /// Invalidates all pre-existing element pointers if capacity must be
        /// increased to accommodate the new elements.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn addManyAt(self: *Self, gpa: Allocator, index: usize, count: usize) Allocator.Error![]T {
            const old_len = self.len;
            const new_len = try addOrOom(old_len, count);

            if (self.capacity >= new_len)
                return self.addManyAtAssumeCapacity(index, count);

            const new_cap = try capacityOrOom(new_len);
            const old_mem = self.entireSlice();

            // We want to avoid unecessary copies, so if we have a slice, we'll
            // first try to remap the old memory into the new capacity.
            if (self.hasAllocation()) {
                if (gpa.remap(old_mem, new_cap)) |new_mem| {
                    self.as.slice = new_mem;
                    self.capacity = new_cap;
                    return self.addManyAtAssumeCapacity(index, count);
                }
            }

            // Otherwise we'll implement our own realloc that avoids the needless
            // copying of the undefined region of the old memory.
            const new_mem = try gpa.alignedAlloc(T, alignment, try capacityOrOom(new_len));
            const to_move = old_mem[index..old_len];
            @memcpy(new_mem[0..index], old_mem[0..index]);
            @memcpy(new_mem[index + count ..][0..to_move.len], to_move);
            self.deinit(gpa);
            self.* = .{ .as = .{ .slice = new_mem }, .len = new_len, .capacity = new_cap };
            return new_mem[index..][0..count];
        }

        /// Add `count` new elements at position `index`, which have
        /// `undefined` values. Returns a slice pointing to the newly allocated
        /// elements, which becomes invalid after various `SmallArrayList`
        /// operations.
        /// Invalidates pre-existing pointers to elements at and after `index`, but
        /// does not invalidate any before that.
        /// Asserts that the list has capacity for the additional items.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn addManyAtAssumeCapacity(self: *Self, index: usize, count: usize) []T {
            const old_len = self.len;
            const new_len = @as(Size, @truncate(old_len + count));
            assert(self.capacity >= new_len);
            const full = self.entireSlice();
            mem.copyBackwards(T, full[index + count ..], full[index..old_len]);
            const result = full[index..][0..count];
            @memset(result, undefined);
            self.len = new_len;
            return result;
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is an array pointing to the newly allocated elements.
        /// The returned pointer becomes invalid when the list is resized.
        /// Resizes list if `self.capacity()` is not large enough.
        pub fn addManyAsArray(self: *Self, gpa: Allocator, comptime n: usize) Allocator.Error!*[n]T {
            const old_len = self.len;
            try self.resize(gpa, old_len + n);
            return self.entireSlice()[old_len..][0..n];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is an array pointing to the newly allocated elements.
        /// Never invalidates element pointers.
        /// The returned pointer becomes invalid when the list is resized.
        /// Asserts that the list can hold the additional items.
        pub fn addManyAsArrayAssumeCapacity(self: *Self, comptime n: usize) *[n]T {
            assert(self.len + n <= self.capacity);
            const old_len = self.len;
            self.len = @truncate(old_len + n);
            return self.entireSlice()[old_len..][0..n];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is a slice pointing to the newly allocated elements.
        /// The returned pointer becomes invalid when the list is resized.
        /// Resizes list if `self.capacity()` is not large enough.
        pub fn addManyAsSlice(self: *Self, gpa: Allocator, n: usize) Allocator.Error![]T {
            const old_len = self.len;
            const new_len = old_len + n;
            try self.resize(gpa, new_len);
            return self.entireSlice()[old_len..old_len];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is a slice pointing to the newly allocated elements.
        /// Never invalidates element pointers.
        /// The returned pointer becomes invalid when the list is resized.
        /// Asserts that the list can hold the additional items.
        pub fn addManyAsSliceAssumeCapacity(self: *Self, n: usize) []T {
            assert(self.len + n <= self.capacity);
            const old_len = self.len;
            self.len = old_len + n;
            return self.entireSlice()[old_len..][0..n];
        }

        /// Adjust the list length to `new_len`.
        /// Additional elements contain the value `undefined`.
        /// Invalidates element pointers if additional memory is needed.
        pub fn resize(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            try self.ensureTotalCapacity(gpa, new_len);
            self.len = @truncate(new_len);
        }

        /// Reduce allocated capacity to `new_len`.
        /// May invalidate element pointers.
        /// Asserts that the new length is less than or equal to the previous length.
        pub fn shrinkAndFree(self: *Self, gpa: Allocator, new_len: usize) void {
            assert(new_len <= self.len);

            const old_mem = self.entireSlice();

            if (new_len <= smallCapacity) {
                if (self.hasAllocation()) {
                    @memcpy(self.as.array[0..new_len], old_mem[0..new_len]);
                    gpa.free(old_mem);
                }
                self.len = @truncate(new_len);
                self.capacity = smallCapacity;
                return;
            }

            if (gpa.remap(old_mem, new_len)) |new_mem| {
                self.as.slice = new_mem;
                self.len = @truncate(new_len);
                self.capacity = @truncate(new_len);
                return;
            }

            const new_mem = gpa.alignedAlloc(T, alignment, new_len) catch |e| switch (e) {
                error.OutOfMemory => {
                    // No problem, capacity is still correct then.
                    self.len = @truncate(new_len);
                    return;
                },
            };

            @memcpy(new_mem, old_mem[0..new_len]);
            gpa.free(old_mem);
            self.as.slice = new_mem;
            self.len = @truncate(new_len);
            self.capacity = @truncate(new_len);
        }

        /// Reduce length to `new_len`.
        /// Invalidates pointers to elements `items[new_len..]`.
        /// Keeps capacity the same.
        /// Asserts that the new length is less than or equal to the previous length.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            assert(new_len <= self.len);
            self.len = @truncate(new_len);
        }

        /// Invalidates all element pointers.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.len = 0;
        }

        /// Invalidates all element pointers.
        pub fn clearAndFree(self: *Self, gpa: Allocator) void {
            self.deinit(gpa);
            self.* = empty;
        }

        /// Modify the array so that it can hold at least `additional_count` **more** items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional_count: usize) Allocator.Error!void {
            return self.ensureTotalCapacity(gpa, try addOrOom(self.len, additional_count));
        }

        /// Increases the array's length to match the full capacity that is already allocated.
        /// The new elements have `undefined` values.
        /// Never invalidates element pointers.
        pub fn expandToCapacity(self: *Self) void {
            self.len = self.capacity;
        }

        /// Modify the array so that it can hold at least `new_capacity` items.
        /// Implements super-linear growth to achieve amortized O(1) append operations.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureTotalCapacity(self: *Self, gpa: Allocator, new_capacity: usize) Allocator.Error!void {
            if (self.capacity >= new_capacity) return;
            return self.grow(gpa, try capacityOrOom(try sizeOrOom(new_capacity)));
        }

        /// If the current capacity is less than `new_capacity`, this function will
        /// modify the array so that it can hold exactly `new_capacity` items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureTotalCapacityPrecise(self: *Self, gpa: Allocator, new_capacity: usize) Allocator.Error!void {
            if (self.capacity >= new_capacity) return;
            return self.grow(gpa, try sizeOrOom(new_capacity));
        }

        /// Increase the capacity of the underlying storage to `new_capacity`, which is
        /// asserted to be greater than the current `self.capacity`.
        fn grow(self: *Self, gpa: Allocator, new_capacity: Size) Allocator.Error!void {
            assert(new_capacity > self.capacity);
            const len = self.len;
            const old_mem = self.entireSlice();

            if (self.hasAllocation()) {
                if (gpa.remap(old_mem, new_capacity)) |new_mem| {
                    self.as.slice = new_mem;
                    self.capacity = new_capacity;
                    return;
                }
            }

            const new_mem = try gpa.alignedAlloc(T, alignment, new_capacity);
            @memcpy(new_mem[0..len], old_mem[0..len]);
            self.deinit(gpa);
            self.as.slice = new_mem;
            self.capacity = new_capacity;
        }

        pub const WriterContext = struct {
            self: *Self,
            allocator: Allocator,
        };

        pub const Writer = if (T != u8)
            @compileError("The Writer interface is only defined for SmallArrayList(u8) " ++
                "but the given type is SmallArrayList(" ++ @typeName(T) ++ ")")
        else
            std.io.Writer(WriterContext, Allocator.Error, appendWrite);

        /// Initializes a Writer which will append to the list.
        pub fn writer(self: *Self, gpa: Allocator) Writer {
            return .{ .context = .{ .self = self, .allocator = gpa } };
        }

        /// Same as `append` except it returns the number of bytes written,
        /// which is always the same as `m.len`. The purpose of this function
        /// existing is to match `std.io.Writer` API.
        /// Invalidates element pointers if additional memory is needed.
        fn appendWrite(context: WriterContext, m: []const u8) Allocator.Error!usize {
            try context.self.appendSlice(context.allocator, m);
            return m.len;
        }

        pub const FixedWriter = std.io.Writer(*Self, Allocator.Error, appendWriteFixed);

        /// Initializes a Writer which will append to the list but will return
        /// `error.OutOfMemory` rather than increasing capacity.
        pub fn fixedWriter(self: *Self) FixedWriter {
            return .{ .context = self };
        }

        /// The purpose of this function existing is to match `std.io.Writer` API.
        fn appendWriteFixed(self: *Self, m: []const u8) error{OutOfMemory}!usize {
            const available_capacity = self.capacity - self.len;
            if (m.len > available_capacity) return error.OutOfMemory;
            self.appendSliceAssumeCapacity(m);
            return m.len;
        }
    };
}

/// Integer addition returning `error.OutOfMemory` on overflow.
fn addOrOom(a: Size, b: usize) Allocator.Error!Size {
    const result, const overflow = @addWithOverflow(a, try sizeOrOom(b));
    if (overflow != 0) return Allocator.Error.OutOfMemory;
    return result;
}

/// Converts a usize into a Size, returning `error.OutOfMemory` on overflow.
fn sizeOrOom(s: usize) Allocator.Error!Size {
    return if (s > math.maxInt(Size)) Allocator.Error.OutOfMemory else @truncate(s);
}

/// Calculate the next capacity size returning `error.OutOfMemory` on overflow.
fn capacityOrOom(n: Size) Allocator.Error!Size {
    return ceilPowerOfTwo(Size, n) catch return Allocator.Error.OutOfMemory;
}

test "init" {
    {
        const list: SmallArrayList(i32) = .empty;

        try testing.expect(list.len == 0);
        try testing.expect(list.capacity == @sizeOf([]i32) / @sizeOf(i32));
        try testing.expect(!list.hasAllocation());
    }
    {
        const list: SmallArrayListSized(i32, 8) = .empty;

        try testing.expect(list.len == 0);
        try testing.expect(list.capacity == 8);
        try testing.expect(!list.hasAllocation());
    }
}

test "initCapacity" {
    {
        const a = testing.allocator;
        var list = try SmallArrayList(u8).initCapacity(a, 200);
        defer list.deinit(a);
        try testing.expect(list.len == 0);
        try testing.expect(list.capacity >= 200);
    }
    {
        const a = testing.allocator;
        var list = try SmallArrayListSized(u8, 8).initCapacity(a, 6);
        try testing.expect(list.len == 0);
        try testing.expect(list.capacity >= 6);
        try testing.expect(!list.hasAllocation());
    }
}

test "clone" {
    {
        const a = testing.allocator;

        var array: SmallArrayList(i32) = .empty;
        try array.append(a, -1);
        try array.append(a, 3);

        var cloned = try array.clone(a);

        try testing.expectEqualSlices(i32, array.items(), cloned.items());
        try testing.expect(cloned.capacity >= array.capacity);

        try testing.expectEqual(@as(i32, -1), cloned.items()[0]);
        try testing.expectEqual(@as(i32, 3), cloned.items()[1]);
    }
    {
        const a = testing.allocator;

        var array: SmallArrayList(i32) = .empty;
        try array.append(a, -1);
        try array.append(a, 3);
        try array.append(a, 5);
        try array.append(a, 7);
        try array.append(a, 9);

        var cloned = try array.clone(a);
        defer cloned.deinit(a);

        try testing.expectEqualSlices(i32, array.items(), cloned.items());
        try testing.expect(cloned.capacity >= array.capacity);

        array.deinit(a);

        try testing.expectEqual(@as(i32, -1), cloned.items()[0]);
        try testing.expectEqual(@as(i32, 3), cloned.items()[1]);
        try testing.expectEqual(@as(i32, 5), cloned.items()[2]);
        try testing.expectEqual(@as(i32, 7), cloned.items()[3]);
        try testing.expectEqual(@as(i32, 9), cloned.items()[4]);
    }
}

test "size" {
    const List = SmallArrayList(i32);

    try testing.expectEqual((@sizeOf(usize) * 2) / 4, List.smallCapacity);
    try testing.expectEqual(3 * @sizeOf(usize), @sizeOf(List));
    try testing.expectEqual(2 * @sizeOf(List), @sizeOf([2]List));
}

test "basic" {
    const a = testing.allocator;
    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    {
        var i: usize = 0;
        while (i < 10) : (i += 1) {
            list.append(a, @as(i32, @intCast(i + 1))) catch unreachable;
        }
    }

    {
        var i: usize = 0;
        while (i < 10) : (i += 1) {
            try testing.expect(list.items()[i] == @as(i32, @intCast(i + 1)));
        }
    }

    for (list.items(), 0..) |v, i| {
        try testing.expect(v == @as(i32, @intCast(i + 1)));
    }

    try testing.expect(list.pop() == 10);
    try testing.expect(list.len == 9);

    list.appendSlice(a, &[_]i32{ 1, 2, 3 }) catch unreachable;
    try testing.expect(list.len == 12);
    try testing.expect(list.pop() == 3);
    try testing.expect(list.pop() == 2);
    try testing.expect(list.pop() == 1);
    try testing.expect(list.len == 9);

    var unaligned: [3]i32 align(1) = [_]i32{ 4, 5, 6 };
    list.appendUnalignedSlice(a, &unaligned) catch unreachable;
    try testing.expect(list.len == 12);
    try testing.expect(list.pop() == 6);
    try testing.expect(list.pop() == 5);
    try testing.expect(list.pop() == 4);
    try testing.expect(list.len == 9);

    list.appendSlice(a, &[_]i32{}) catch unreachable;
    try testing.expect(list.len == 9);

    // can only set on indices < self.items.len
    list.items()[7] = 33;
    list.items()[8] = 42;

    try testing.expect(list.pop() == 42);
    try testing.expect(list.pop() == 33);
}

test "appendSlice" {
    const a = testing.allocator;
    var list: SmallArrayList(u8) = .empty;
    defer list.deinit(a);

    try list.appendSlice(a, "abcdefg");
    try testing.expect(!list.hasAllocation());
    try list.appendSlice(a, "hijklmn");
    try testing.expect(!list.hasAllocation());
    try list.appendSlice(a, "opqrstu");
    try testing.expect(list.hasAllocation());
    try testing.expectEqualStrings("abcdefghijklmnopqrstu", list.items());
}

test "appendNTimes" {
    const a = testing.allocator;
    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.appendNTimes(a, 2, 10);
    try testing.expectEqual(@as(usize, 10), list.len);
    for (list.items()) |element| {
        try testing.expectEqual(@as(i32, 2), element);
    }
}

test "appendNTimes with failing allocator" {
    const a = testing.failing_allocator;
    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);
    try testing.expectError(error.OutOfMemory, list.appendNTimes(a, 2, 10));
}

test "orderedRemove" {
    const a = testing.allocator;
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);

        try list.append(a, 1);
        try list.append(a, 2);
        try list.append(a, 3);
        try list.append(a, 4);
        try list.append(a, 5);
        try list.append(a, 6);
        try list.append(a, 7);

        //remove from middle
        try testing.expectEqual(@as(i32, 4), list.orderedRemove(3));
        try testing.expectEqual(@as(i32, 5), list.items()[3]);
        try testing.expectEqual(@as(usize, 6), list.len);

        //remove from end
        try testing.expectEqual(@as(i32, 7), list.orderedRemove(5));
        try testing.expectEqual(@as(usize, 5), list.len);

        //remove from front
        try testing.expectEqual(@as(i32, 1), list.orderedRemove(0));
        try testing.expectEqual(@as(i32, 2), list.items()[0]);
        try testing.expectEqual(@as(usize, 4), list.len);
    }
    {
        // remove last item
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.append(a, 1);
        try testing.expectEqual(@as(i32, 1), list.orderedRemove(0));
        try testing.expectEqual(@as(usize, 0), list.len);
    }
}

test "swapRemove" {
    const a = testing.allocator;
    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.append(a, 1);
    try list.append(a, 2);
    try list.append(a, 3);
    try list.append(a, 4);
    try list.append(a, 5);
    try list.append(a, 6);
    try list.append(a, 7);

    //remove from middle
    try testing.expect(list.swapRemove(3) == 4);
    try testing.expect(list.items()[3] == 7);
    try testing.expect(list.len == 6);

    //remove from end
    try testing.expect(list.swapRemove(5) == 6);
    try testing.expect(list.len == 5);

    //remove from front
    try testing.expect(list.swapRemove(0) == 1);
    try testing.expect(list.items()[0] == 5);
    try testing.expect(list.len == 4);
}

test "insert" {
    const a = testing.allocator;
    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.insert(a, 0, 1);
    try list.append(a, 2);
    try list.insert(a, 2, 3);
    try list.insert(a, 0, 5);
    try testing.expect(list.items()[0] == 5);
    try testing.expect(list.items()[1] == 1);
    try testing.expect(list.items()[2] == 2);
    try testing.expect(list.items()[3] == 3);
}

test "insertSlice" {
    const a = testing.allocator;
    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.append(a, 1);
    try list.append(a, 2);
    try list.append(a, 3);
    try list.append(a, 4);
    try list.insertSlice(a, 1, &[_]i32{ 9, 8 });
    try testing.expect(list.items()[0] == 1);
    try testing.expect(list.items()[1] == 9);
    try testing.expect(list.items()[2] == 8);
    try testing.expect(list.items()[3] == 2);
    try testing.expect(list.items()[4] == 3);
    try testing.expect(list.items()[5] == 4);

    const items = [_]i32{1};
    try list.insertSlice(a, 0, items[0..0]);
    try testing.expect(list.len == 6);
    try testing.expect(list.items()[0] == 1);
}

test "SmallArrayList.replaceRange" {
    const a = testing.allocator;

    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(a, 1, 0, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 2, 3, 4, 5 }, list.items());
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(a, 1, 1, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(
            i32,
            &[_]i32{ 1, 0, 0, 0, 3, 4, 5 },
            list.items(),
        );
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(a, 1, 2, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 4, 5 }, list.items());
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(a, 1, 3, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 5 }, list.items());
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(a, 1, 4, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0 }, list.items());
    }
}

test "SmallArrayList.replaceRangeAssumeCapacity" {
    const a = testing.allocator;

    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 0, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 2, 3, 4, 5 }, list.items());
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 1, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(
            i32,
            &[_]i32{ 1, 0, 0, 0, 3, 4, 5 },
            list.items(),
        );
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 2, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 4, 5 }, list.items());
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 3, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 5 }, list.items());
    }
    {
        var list: SmallArrayList(i32) = .empty;
        defer list.deinit(a);
        try list.appendSlice(a, &[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 4, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0 }, list.items());
    }
}

test "shrink still sets length when resizing is disabled" {
    var failing_allocator = testing.FailingAllocator.init(testing.allocator, .{ .resize_fail_index = 0 });
    const a = failing_allocator.allocator();

    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.append(a, 1);
    try list.append(a, 2);
    try list.append(a, 3);

    list.shrinkAndFree(a, 1);
    try testing.expect(list.len == 1);
}

test "shrinkAndFree with a copy" {
    var failing_allocator = testing.FailingAllocator.init(testing.allocator, .{ .resize_fail_index = 0 });
    const a = failing_allocator.allocator();

    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.appendNTimes(a, 3, 16);
    list.shrinkAndFree(a, 4);
    try testing.expect(mem.eql(i32, list.items(), &.{ 3, 3, 3, 3 }));
}

test "addManyAsArray" {
    const a = std.testing.allocator;
    var list: SmallArrayList(u8) = .empty;
    defer list.deinit(a);

    (try list.addManyAsArray(a, 4)).* = "aoeu".*;
    try list.ensureTotalCapacity(a, 8);
    list.addManyAsArrayAssumeCapacity(4).* = "asdf".*;

    try testing.expectEqualSlices(u8, "aoeuasdf", list.items());
}

test "growing memory preserves contents" {
    // Shrink the list after every insertion to ensure that a memory growth
    // will be triggered in the next operation.
    const a = std.testing.allocator;
    var list: SmallArrayList(u8) = .empty;
    defer list.deinit(a);

    (try list.addManyAsArray(a, 4)).* = "abcd".*;
    list.shrinkAndFree(a, 4);

    try list.appendSlice(a, "efgh");
    try testing.expectEqualSlices(u8, "abcdefgh", list.items());
    list.shrinkAndFree(a, 8);

    try list.insertSlice(a, 4, "ijkl");
    try testing.expectEqualSlices(u8, "abcdijklefgh", list.items());
}

test "accepts unaligned slices" {
    const a = testing.allocator;
    var list: SmallArrayListAligned(u8, 8) = .empty;
    defer list.deinit(a);

    try list.appendSlice(a, &.{ 0, 1, 2, 3 });
    try list.insertSlice(a, 2, &.{ 4, 5, 6, 7 });
    try list.replaceRange(a, 1, 3, &.{ 8, 9 });

    try testing.expectEqualSlices(u8, &.{ 0, 8, 9, 6, 7, 2, 3 }, list.items());
    try testing.expect(!list.hasAllocation());
}

test "SmallArrayList(?u32).pop()" {
    const a = testing.allocator;

    var list: SmallArrayList(?i32) = .empty;
    defer list.deinit(a);

    try list.append(a, null);
    try list.append(a, 1);
    try list.append(a, 2);
    try testing.expectEqual(list.len, 3);

    try testing.expect(list.pop().? == @as(u32, 2));
    try testing.expect(list.pop().? == @as(u32, 1));
    try testing.expect(list.pop().? == null);
    try testing.expect(list.pop() == null);
}

test "SmallArrayList(u32).getLast()" {
    const a = testing.allocator;

    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try list.append(a, 2);
    const const_list = list;
    try testing.expectEqual(const_list.getLast(), 2);
}

test "SmallArrayList(u32).getLastOrNull()" {
    const a = testing.allocator;

    var list: SmallArrayList(i32) = .empty;
    defer list.deinit(a);

    try testing.expectEqual(list.getLastOrNull(), null);

    try list.append(a, 2);
    const const_list = list;
    try testing.expectEqual(const_list.getLastOrNull().?, 2);
}

test "return OutOfMemory when capacity would exceed maximum usize integer value" {
    const a = testing.allocator;
    const new_item: u32 = 42;
    const items = &.{ 42, 43 };

    var list: SmallArrayList(i32) = .{
        .as = undefined,
        .len = math.maxInt(Size) - 1,
        .capacity = math.maxInt(Size) - 1,
    };

    try testing.expectError(error.OutOfMemory, list.appendSlice(a, items));
    try testing.expectError(error.OutOfMemory, list.appendNTimes(a, new_item, 2));
    try testing.expectError(error.OutOfMemory, list.appendUnalignedSlice(a, &.{ new_item, new_item }));
    try testing.expectError(error.OutOfMemory, list.addManyAt(a, 0, 2));
    try testing.expectError(error.OutOfMemory, list.addManyAsArray(a, 2));
    try testing.expectError(error.OutOfMemory, list.addManyAsSlice(a, 2));
    try testing.expectError(error.OutOfMemory, list.insertSlice(a, 0, items));
    try testing.expectError(error.OutOfMemory, list.ensureUnusedCapacity(a, 2));
}

test "sized" {
    const a = testing.allocator;

    var list1: SmallArrayList(i32) = .empty;
    var list2: SmallArrayListSized(i32, 6) = .empty;

    defer list1.deinit(a);
    // don't strictly need to deinit list2

    for (0..@TypeOf(list2).smallCapacity) |i| {
        try list1.append(a, @intCast(i));
        try list2.append(a, @intCast(i));
    }

    try testing.expect(list1.hasAllocation());
    try testing.expect(!list2.hasAllocation());
}

test "expand and shrink" {
    const List = SmallArrayList(u8);
    var list: List = .empty;
    try testing.expectEqual(list.len, 0);
    try testing.expectEqual(list.capacity, List.smallCapacity);

    list.expandToCapacity();
    try testing.expectEqual(list.len, List.smallCapacity);
    try testing.expectEqual(list.capacity, List.smallCapacity);

    list.shrinkRetainingCapacity(4);
    try testing.expectEqual(list.len, 4);
    try testing.expectEqual(list.capacity, List.smallCapacity);

    list.clearRetainingCapacity();
    try testing.expectEqual(list.len, 0);
    try testing.expectEqual(list.capacity, List.smallCapacity);
}

test "ensureTotalCapacity" {
    const List = SmallArrayList(u8);
    const a = testing.allocator;
    var list: List = .empty;

    try list.ensureTotalCapacity(a, 100);

    try testing.expectEqual(list.len, 0);
    try testing.expectEqual(list.capacity, 128);

    list.clearAndFree(a);

    try testing.expectEqual(list.len, 0);
    try testing.expectEqual(list.capacity, List.smallCapacity);
}

test "SmallArrayList(u8) implements writer" {
    const a = testing.allocator;

    {
        var buffer: SmallArrayList(u8) = .empty;
        defer buffer.deinit(a);

        const x: i32 = 42;
        const y: i32 = 1234;
        try buffer.writer(a).print("x: {}\ny: {}\n", .{ x, y });

        try testing.expectEqualSlices(u8, "x: 42\ny: 1234\n", buffer.items());
    }
    {
        var list: SmallArrayListAligned(u8, 2) = .empty;
        defer list.deinit(a);

        const writer = list.writer(a);
        try writer.writeAll("a");
        try writer.writeAll("bc");
        try writer.writeAll("d");
        try writer.writeAll("efg");

        try testing.expectEqualSlices(u8, "abcdefg", list.items());
    }
}

test "SmallArrayList(u8) implements fixedWriter" {
    var buffer: SmallArrayList(u8) = .empty;

    const x: i32 = 42;
    const y: i32 = 1234;
    try buffer.fixedWriter().print("x: {}\ny: {}\n", .{ x, y });

    try testing.expectEqualSlices(u8, "x: 42\ny: 1234\n", buffer.items());
    try testing.expectError(error.OutOfMemory, buffer.fixedWriter().print("x: {}\ny: {}\n", .{ x, y }));
}
