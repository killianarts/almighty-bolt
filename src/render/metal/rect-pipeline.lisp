(defpackage #:almighty-bolt/render/metal/rect-pipeline
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:import-from #:almighty-bolt/render/metal/device
                #:compile-library
                #:make-function
                #:make-pipeline-state)
  (:import-from #:almighty-bolt/clock
                #:now-ns)
  (:export #:+instance-bytes+
           #:+shader-source+
           #:make-rect-pipeline))

(in-package #:almighty-bolt/render/metal/rect-pipeline)

;;; The pipeline that draws rectangles, rounded rectangles and glyphs. Every
;;; command is one instance of a four-vertex strip. For a shape the vertex shader
;;; places the quad one pixel larger than the shape all round and the fragment
;;; shader covers the shape by its signed distance, so that edges and corners are
;;; antialiased and a plain rectangle is a rounded one with radius zero. For a
;;; glyph the quad is its bitmap, and the fragment shader reads the coverage from
;;; the glyph atlas. Colours are premultiplied by coverage and blended with
;;; "source over".

(defconstant +instance-bytes+ 32
  "The size of one instance: four floats for the rectangle, a float parameter, then
the colour, the kind and a pad word, as unsigned 32-bit words.")

(defparameter +shader-source+
  "#include <metal_stdlib>
using namespace metal;

struct Instance {
  float4 rect;      // x, y, width, height in points, from the top left
  float parameter;  // the corner radius in points, or a glyph's index in the atlas
  uint color;       // red, green, blue, alpha: a byte each, red in the high byte
  uint kind;        // 0 rectangle, 1 rounded rectangle, 2 glyph
  uint pad;
};

struct Frame {
  float2 viewport;  // the size of the drawing area in points
  float scale;      // pixels per point
  float pad;
};

struct VertexOut {
  float4 position [[position]];
  float2 local;                    // offset from the shape's centre, in points
  float2 uv;                       // texture coordinates, for a glyph
  float2 half_size [[flat]];
  float radius [[flat]];
  float scale [[flat]];
  float4 color [[flat]];
  uint kind [[flat]];
};

vertex VertexOut rect_vertex(uint vertex_id [[vertex_id]],
                             uint instance_id [[instance_id]],
                             constant Instance *instances [[buffer(0)]],
                             constant Frame &frame [[buffer(1)]],
                             constant float4 *glyph_uvs [[buffer(2)]]) {
  Instance inst = instances[instance_id];
  bool glyph = inst.kind == 2u;
  float2 corner = float2(float(vertex_id & 1), float(vertex_id >> 1));
  float2 half_size = inst.rect.zw * 0.5;
  // A shape gets one more pixel all round for its antialiased edge; a glyph's
  // bitmap already has its own margin.
  float margin = glyph ? 0.0 : 1.0 / frame.scale;
  float2 local = (corner * 2.0 - 1.0) * (half_size + margin);
  float2 point = inst.rect.xy + half_size + local;
  uint c = inst.color;
  VertexOut out;
  out.position = float4(point.x / frame.viewport.x * 2.0 - 1.0,
                        1.0 - point.y / frame.viewport.y * 2.0, 0.0, 1.0);
  out.local = local;
  out.uv = float2(0.0);
  if (glyph) {
    float4 uvs = glyph_uvs[uint(inst.parameter)];   // left, top, right, bottom
    out.uv = mix(uvs.xy, uvs.zw, corner);
  }
  out.half_size = half_size;
  out.radius = glyph ? 0.0 : min(inst.parameter, min(half_size.x, half_size.y));
  out.scale = frame.scale;
  out.color = float4(float((c >> 24) & 255u), float((c >> 16) & 255u),
                     float((c >> 8) & 255u), float(c & 255u)) / 255.0;
  out.kind = inst.kind;
  return out;
}

fragment float4 rect_fragment(VertexOut in [[stage_in]],
                              texture2d<float> atlas [[texture(0)]]) {
  float coverage;
  if (in.kind == 2u) {
    // The atlas is at device resolution, so sample it texel for pixel.
    constexpr sampler nearest(coord::normalized, filter::nearest, address::clamp_to_edge);
    coverage = atlas.sample(nearest, in.uv, level(0.0)).r;
  } else {
    // Signed distance to a rounded box: negative inside, zero on the edge.
    float2 q = abs(in.local) - (in.half_size - in.radius);
    float d = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - in.radius;
    coverage = clamp(0.5 - d * in.scale, 0.0, 1.0);
  }
  float alpha = in.color.a * coverage;
  return float4(in.color.rgb * alpha, alpha);
}
"
  "The Metal Shading Language source of the rectangle and glyph pipeline.")

(defconstant +blend-one+ 1)
(defconstant +blend-one-minus-source-alpha+ 5)

(defun make-rect-pipeline (device pixel-format)
  "Compile the shaders and make the pipeline state for drawing into textures of
PIXEL-FORMAT. Returns the state and, for measuring, the nanoseconds the shader
compiler and the pipeline took, as three values."
  (let* ((start (now-ns))
         (library (compile-library device +shader-source+))
         (compiled (now-ns))
         (vertex (make-function library "rect_vertex"))
         (fragment (make-function library "rect_fragment"))
         (descriptor (msg-send :id (msg-send :id (objc-class "MTLRenderPipelineDescriptor")
                                             "alloc")
                               "init"))
         (attachment (msg-send :id (msg-send :id descriptor "colorAttachments")
                               "objectAtIndexedSubscript:" (:ulong 0))))
    (msg-send :void descriptor "setVertexFunction:" (:id vertex))
    (msg-send :void descriptor "setFragmentFunction:" (:id fragment))
    (msg-send :void attachment "setPixelFormat:" (:ulong pixel-format))
    (msg-send :void attachment "setBlendingEnabled:" (:bool t))
    (msg-send :void attachment "setSourceRGBBlendFactor:" (:ulong +blend-one+))
    (msg-send :void attachment "setDestinationRGBBlendFactor:"
              (:ulong +blend-one-minus-source-alpha+))
    (msg-send :void attachment "setSourceAlphaBlendFactor:" (:ulong +blend-one+))
    (msg-send :void attachment "setDestinationAlphaBlendFactor:"
              (:ulong +blend-one-minus-source-alpha+))
    (let ((state (make-pipeline-state device descriptor)))
      (values state (- compiled start) (- (now-ns) compiled)))))
