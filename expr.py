class OPTAttention(nn.Module):
    def __init__(
        self
    ):
        self.k_proj = nn.Linear(embed_dim, embed_dim, bias=bias)
