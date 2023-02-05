class Test():
    def __init__(self):
        pass
    def forward(e):
        if attn_weights.dtype == torch.float16:
            attn_weights = nn.functional.softmax(attn_weights, dim=-1, dtype=torch.float32).to(torch.float16)
        else:
            attn_weights = nn.functional.softmax(attn_weights, dim=-1)
#         attn_weights = torch.nn.functional.softmax(attn_weights, dim=-1, dtype=torch.float32).to(torch.float16)
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
