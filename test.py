class Test():
    def __init__(self):
        pass
    def forward(e):
        attn_weights = torch.bmm(query_states, key_states.transpose(1, 2))
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
