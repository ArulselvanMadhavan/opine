class OPTAttention(Test):
    def __init__(self, embed_dim : bool, n_dim=32):
        super(Test, self).__init__(embed_dim, n_dim)

    def forward(self, embed_dim):
        if not self.run_on_envise:
            return super(Test,self).forward(embed_dim)
        return super(Test,self).forward(embed_dim)
