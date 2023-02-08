class Sample():
    def __init__(self, embed_dim, backend=None):
        self.embed_dim = embed_dim
        self.backend = backend
        
    def forward(self, weight, value):
        return torch.bmm(weight, value)
