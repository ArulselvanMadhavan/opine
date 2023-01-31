class OPTAttention(nn.Module):
    """Multi-headed attention from 'Attention Is All You Need' paper"""

    def __init__(
        self,
        embed_dim: int,
        num_heads: int,
        dropout: float = 0.,
        is_decoder: bool = False,
        bias: bool = True,
    ):
        self.embed_dim = embed_dim
