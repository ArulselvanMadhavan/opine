# def tiled_nd_gemm(self, weight, value):
#     """Tiled GEMM allowing 1-d inputs.
#     Arguments:
#         weight (torch.Tensor): a 3d or 4d weight tensor
#         value (torch.Tensor): a value vector or matrix that's shape
#             compatible with matrix multiplication along the last two
#             dimensions of the weight matrix
#     """
#     if not self.run_on_envise:
#         return torch.matmul(weight, value)

#     orig_value = value
#     if orig_value.ndim == 1:
#         value = value.reshape(-1, 1)
#     output = TiledGEMM.apply(
#         self.backend,
#         weight,
#         value,
#         self.quantized_backward,
#         self.layer_repeat,
#     )
#     if orig_value.ndim == 1:
#         output = output.squeeze(-1)
#     return output
class Test():
    def __init__(self):
        pass
    def forward(e):
        attn_output = unshape(torch.matmul(attn_weights, value_states)) 
        #         attn_output = torch.bmm(attn_probs, value_states)
#         attn_output = unshape(torch.matmul(attn_weights, value_states))
#         attn_weights = nn.functional.softmax(scores.float(), dim=-1).type_as(
#             scores
#         )         
# qnn.Softmax(
#             dim=-1,
#             run_on_envise=run_on_envise,
#             backend=self.backend,
#             softmax_type=softmax_type,
#         )(attn_weights)
# .to(torch.float16)

# def bmm(self, weight, value):
#     if not self.run_on_envise:
#         return torch.bmm(weight, value)

#     return bmm(self.backend, weight, value)
# class Test():
#     def __init__(self, backend):
#         self.k_proj = nn.Linear(embed_dim, embed_dim, bias=bias)
#     def forward(self):
#         self.test = 1
#assign_param(qattn.k_proj, mod.k_proj, "weight")
#self.backend = deepcopy(backend) or Envise()
