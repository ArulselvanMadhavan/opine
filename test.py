class OPTAttention(Test):
    def __init__(self, embed_dim : bool, n_dim=32):
        self._embed_dim = embed_dim

    def forward(self):
        attn_out = torch.matmul(x,w)
